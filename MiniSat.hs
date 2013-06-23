-- Haskell bindings for MiniSat 1.14 (proof logging version)
-- based in part on https://github.com/msakai/haskell-minisat

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module MiniSat 
    ( Var(..)
    , Literal(..)
    , Clause
    , neg
    , isNeg
    , mapLit
    , compLit
    , var
    , encodeLit
    , decodeLit

    , Solver
    , runSolver
    , newVar
    , nVars
    , addClause
    , addUnit
    , addBinary
    , addTernary
    , solve
    , isOkay

    , ProofNode
    , ClauseId
    , proof

      -- * Re-export
    , liftIO
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bits
import Data.IORef
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Foreign
import Foreign.C

import Text.Read (readPrec,parens)
import Text.ParserCombinators.ReadP (string,choice)
import Text.ParserCombinators.ReadPrec ((<++))
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (lift)

import Control.Exception (bracket)

-----------------------------------------------------------------------

newtype Var = Var CVar deriving (Eq, Ord, Num, Enum, Integral, Real)

data Literal = Pos {-# UNPACK #-} !Var
             | Neg {-# UNPACK #-} !Var
             deriving (Eq, Ord)

type Clause = [Literal]

neg :: Literal -> Literal
neg (Pos x) = (Neg x)
neg (Neg x) = (Pos x)

isNeg :: Literal -> Bool
isNeg (Neg x) = True
isNeg _       = False

mapLit :: (Var -> Var) -> Literal -> Literal
mapLit f (Pos x) = (Pos $ f x)
mapLit f (Neg x) = (Neg $ f x)

compLit :: Literal -> Literal -> Literal
compLit a (Pos _) = a
compLit a (Neg _) = neg a

var :: Literal -> Var
var (Pos x) = x
var (Neg x) = x

encodeLit :: Literal -> CLit
encodeLit (Pos (Var v)) = v + v
encodeLit (Neg (Var v)) = v + v + 1

decodeLit :: CLit -> Literal
decodeLit x | x .&. 1 == 0 = Pos v
            | otherwise    = Neg v
    where v = Var $ x `shiftR` 1

-----------------------------------------------------------------------

instance Show Var where
    show (Var n) = show n

instance Read Var where
    readPrec = parens $ readPrec >>= return . Var

instance Show Literal where
    show (Pos n) =     show n
    show (Neg n) = '¬':show n

instance Read Literal where
    readPrec = parens $ do ReadPrec.lift $ choice [string "¬", string "-"]
                           readPrec >>= return . Neg . Var
                    <++ do readPrec >>= return . Pos . Var

-----------------------------------------------------------------------

data SolverEnv = SolverEnv { _solver :: ForeignPtr CSolver
                           , _proof  :: IORef (IOVector ProofNode)
                           , _used   :: IORef Int }

newtype Solver a = Solver (ReaderT SolverEnv IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runSolver :: Solver a -> IO a
runSolver (Solver act) = do
    solver <- newForeignPtr p_solver_delete =<< c_solver_new
    proof <- newIORef =<< VM.new 100 -- TODO: finetune
    used <- newIORef 0
    let env = SolverEnv solver proof used
    rootFunPtr <- mkRootFunPtr (root env)
    chainFunPtr <- mkChainFunPtr (chain env)
    deletedFunPtr <- mkDeletedFunPtr (deleted env)
    t <- withForeignPtr solver $ c_solver_newProof rootFunPtr chainFunPtr deletedFunPtr
    r <- runReaderT act env    
    withForeignPtr solver $ c_solver_deleteProof t
    return r

withSolver :: (Ptr CSolver -> IO a) -> Solver a
withSolver f = Solver $ lift =<< asks (flip withForeignPtr f . _solver)

newVar :: Solver Var
newVar = Var <$> withSolver c_solver_newVar

nVars :: Solver Int
nVars = fromIntegral <$> withSolver c_solver_nVars

addClause :: Clause -> Solver ()
addClause c = withSolver $ \solver -> do
    withArrayLen (map encodeLit c) $ \n arr -> do
        c_solver_addClause solver arr (fromIntegral n)

addUnit :: Literal -> Solver ()
addUnit = withSolver . c_solver_addUnit . encodeLit

addBinary :: Literal -> Literal -> Solver ()
addBinary a b = withSolver $ c_solver_addBinary (encodeLit a) (encodeLit b)

addTernary :: Literal -> Literal -> Literal -> Solver ()
addTernary a b c = 
    withSolver $ c_solver_addTernary (encodeLit a) (encodeLit b) (encodeLit c)

solve :: Solver ()
solve = withSolver c_solver_solve

isOkay :: Solver Bool
isOkay = withSolver c_solver_okay

-----------------------------------------------------------------------

data ProofNode = Root Clause
               | Chain Clause [ClauseId] [Var]
               | Sink [ClauseId] [Var]
               | Deleted
               deriving (Show)

proof :: Solver (Vector ProofNode)
proof = Solver $ do
    p <- liftIO . readIORef =<< asks _proof
    n <- liftIO . readIORef =<< asks _used
    let p' = VM.slice 0 n p
    liftIO $ V.freeze p'

root :: SolverEnv -> Ptr CLit -> CInt -> IO ()
root SolverEnv{..} c c_size = do
    clause <- map decodeLit <$> peekArray (fromIntegral c_size) c
    appendProofNode _proof _used (Root clause)

chain :: SolverEnv -> Ptr CClauseId -> CInt -> Ptr CVar -> CInt -> IO ()
chain SolverEnv{..} cs cs_size xs xs_size = do
    clauseIds <- map fromIntegral <$> peekArray (fromIntegral cs_size) cs
    vars <- map Var <$> peekArray (fromIntegral xs_size) xs
    p <- readIORef _proof
    clauses <- mapM (readClause p) clauseIds
    let node = case resolve clauses vars of
                   []  -> Sink clauseIds vars
                   res -> Chain res clauseIds vars
    appendProofNode _proof _used node

resolve :: [Clause] -> [Var] -> Clause
resolve [c] [] = c
resolve (c:d:xs) (a:ys) = resolve (d':xs) ys
    where d' = filter ((/= a) . var) (c ++ d)

deleted :: SolverEnv -> CClauseId -> IO ()
deleted SolverEnv{..} c = do
    p <- readIORef _proof
    VM.unsafeWrite p (fromIntegral c) Deleted

readClause :: IOVector ProofNode -> ClauseId -> IO Clause
readClause p i = VM.unsafeRead p (fromIntegral i) >>= \case
    Root  c     -> return c
    Chain c _ _ -> return c
    Sink    _ _ -> error "MiniSat.readClause: sink"
    Deleted     -> error "MiniSat.readClause: deleted"

appendProofNode :: IORef (IOVector ProofNode) -> IORef Int -> ProofNode -> IO ()
appendProofNode proof used node = do
    p <- readIORef proof
    i <- readIORef used
    p' <- if i < VM.length p 
            then return p
            else do p' <- VM.unsafeGrow p i
                    writeIORef proof p'
                    return p'    
    VM.unsafeWrite p' i node
    writeIORef used (i+1)

-----------------------------------------------------------------------

data CSolver
type CVar = CInt
type CLit = CInt  

foreign import ccall unsafe "minisat_newSolver"
    c_solver_new :: IO (Ptr CSolver)

foreign import ccall unsafe "& minisat_deleteSolver" 
    p_solver_delete :: FinalizerPtr CSolver

foreign import ccall unsafe "minisat_newVar" 
    c_solver_newVar :: Ptr CSolver -> IO CVar

foreign import ccall unsafe "minisat_nVars" 
    c_solver_nVars :: Ptr CSolver -> IO CInt

foreign import ccall safe "minisat_addClause"
    c_solver_addClause :: Ptr CSolver -> Ptr CLit -> CInt -> IO ()

foreign import ccall safe "minisat_addUnit"
    c_solver_addUnit :: CLit -> Ptr CSolver -> IO ()

foreign import ccall safe "minisat_addBinary"
    c_solver_addBinary :: CLit -> CLit -> Ptr CSolver -> IO ()

foreign import ccall safe "minisat_addTernary"
    c_solver_addTernary :: CLit -> CLit -> CLit -> Ptr CSolver -> IO ()

foreign import ccall safe "minisat_solve"
    c_solver_solve :: Ptr CSolver -> IO ()

foreign import ccall unsafe "minisat_okay"
    c_solver_okay :: Ptr CSolver -> IO Bool

-----------------------------------------------------------------------

type RootCallback = Ptr CLit -> CInt -> IO ()
type ChainCallback = Ptr CClauseId -> CInt -> Ptr CVar -> CInt -> IO ()
type DeletedCallback = CClauseId -> IO ()

foreign import ccall "wrapper"
    mkRootFunPtr :: RootCallback -> IO (FunPtr RootCallback)

foreign import ccall "wrapper"
    mkChainFunPtr :: ChainCallback -> IO (FunPtr ChainCallback)

foreign import ccall "wrapper"
    mkDeletedFunPtr :: DeletedCallback -> IO (FunPtr DeletedCallback)

data CProofTraverser
type ClauseId = CClauseId  --TODO
type CClauseId = CInt

foreign import ccall unsafe "minisat_newProof"
    c_solver_newProof :: FunPtr RootCallback 
                         -> FunPtr ChainCallback 
                         -> FunPtr DeletedCallback
                         -> Ptr CSolver
                         -> IO (Ptr CProofTraverser)

foreign import ccall unsafe "minisat_deleteProof"
    c_solver_deleteProof :: Ptr CProofTraverser -> Ptr CSolver -> IO ()
