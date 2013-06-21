-- Haskell bindings for MiniSat 1.14 (proof logging version)
-- based in part on https://github.com/msakai/haskell-minisat

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module MiniSat 
    ( Var(..)
    , Literal(..)
    , Clause
    , neg
    , isNeg
    , mapLit
    , var
    , varEq

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

    , runSolverWithProofLogging
    , ClauseId

      -- * Re-export
    , liftIO
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bits
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

var :: Literal -> Var
var (Pos x) = x
var (Neg x) = x

varEq :: Literal -> Literal -> Bool
varEq a b = (var a) == (var b)

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

newtype Solver a = Solver (ReaderT (ForeignPtr CSolver) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runSolver :: Solver a -> IO a
runSolver (Solver act) = 
    runReaderT act =<< (newForeignPtr p_solver_delete =<< c_solver_new)

withSolver :: (Ptr CSolver -> IO a) -> Solver a
withSolver f = Solver $ lift =<< asks (flip withForeignPtr f)

newVar :: Solver Var
newVar = Var <$> withSolver c_solver_newVar

nVars :: Solver Int
nVars = fromIntegral <$> withSolver c_solver_nVars

addClause :: Clause -> Solver ()
addClause c = withSolver $ \solver -> do
    bracket c_vecLit_new 
            c_vecLit_delete 
            (\veclit -> do
                pushLits veclit c
                c_solver_addClause solver veclit)
    where
        pushLits :: Ptr CVecLit -> [Literal] -> IO ()
        pushLits p = mapM_ $ \case 
            (Pos (Var v)) -> c_vecLit_pushVar p v 0
            (Neg (Var v)) -> c_vecLit_pushVar p v 1

unlit :: Literal -> (CVar,CInt)
unlit (Pos (Var v)) = (v,0)
unlit (Neg (Var v)) = (v,1)

addUnit :: Literal -> Solver ()
addUnit l = withSolver (c_solver_addUnit v s)
    where (v,s) = unlit l

addBinary :: Literal -> Literal -> Solver ()
addBinary l1 l2 = withSolver (c_solver_addBinary v1 s1 v2 s2)
    where (v1,s1) = unlit l1
          (v2,s2) = unlit l2

addTernary :: Literal -> Literal -> Literal -> Solver ()
addTernary l1 l2 l3 = withSolver (c_solver_addTernary v1 s1 v2 s2 v3 s3)
    where (v1,s1) = unlit l1
          (v2,s2) = unlit l2
          (v3,s3) = unlit l3

solve :: Solver ()
solve = withSolver c_solver_solve

isOkay :: Solver Bool
isOkay = withSolver c_solver_okay

-----------------------------------------------------------------------

runSolverWithProofLogging :: (Clause -> IO ())               -- add root
                          -> ([ClauseId] -> [Var] -> IO ())  -- add chain
                          -> (ClauseId -> IO ())             -- deleted
                          -> Solver a
                          -> IO a
runSolverWithProofLogging root chain deleted solver = do
    
    rootFunPtr <- mkRootFunPtr $ \c -> do
        let size = fromIntegral $ c_vecLit_size c
            dat  = c_vecLit_data c
        lits <- map c2lit <$> peekArray size dat
        root lits

    chainFunPtr <- mkChainFunPtr $ \cs cs_size xs xs_size -> do
        clauseIds <- peekArray (fromIntegral cs_size) cs
        vars <- map Var <$> peekArray (fromIntegral xs_size) xs
        chain clauseIds vars
    
    deletedFunPtr <- mkDeletedFunPtr deleted

    runSolver $ do
        t <- withSolver (c_solver_newProof rootFunPtr chainFunPtr deletedFunPtr)
        r <- solver
        withSolver (c_solver_deleteProof t)
        return r

type RootCallback = Ptr CVecLit -> IO ()
type ChainCallback = Ptr CClauseId -> CInt -> Ptr CVar -> CInt -> IO ()
type DeletedCallback = CClauseId -> IO ()

foreign import ccall "wrapper"
    mkRootFunPtr :: RootCallback -> IO (FunPtr RootCallback)

foreign import ccall "wrapper"
    mkChainFunPtr :: ChainCallback -> IO (FunPtr ChainCallback)

foreign import ccall "wrapper"
    mkDeletedFunPtr :: DeletedCallback -> IO (FunPtr DeletedCallback)

data CProofTraverser

foreign import ccall unsafe "minisat_newProof"
    c_solver_newProof :: FunPtr RootCallback 
                         -> FunPtr ChainCallback 
                         -> FunPtr DeletedCallback
                         -> Ptr CSolver
                         -> IO (Ptr CProofTraverser)

foreign import ccall unsafe "minisat_deleteProof"
    c_solver_deleteProof :: Ptr CProofTraverser -> Ptr CSolver -> IO ()


type ClauseId = CClauseId  --TODO

type CClauseId = CInt
type CLit = CInt

c2lit :: CLit -> Literal
c2lit x | x .&. 1 == 0 = Pos v
        | otherwise    = Neg v
    where v = Var $ x `shiftR` 1

-----------------------------------------------------------------------

data CSolver
type CVar = CInt
data CVecLit

foreign import ccall unsafe "minisat_newSolver"
    c_solver_new :: IO (Ptr CSolver)

foreign import ccall unsafe "& minisat_deleteSolver" 
    p_solver_delete :: FinalizerPtr CSolver

foreign import ccall unsafe "minisat_newVar" 
    c_solver_newVar :: Ptr CSolver -> IO CVar

foreign import ccall unsafe "minisat_nVars" 
    c_solver_nVars :: Ptr CSolver -> IO CInt

foreign import ccall unsafe "minisat_addClause"
    c_solver_addClause :: Ptr CSolver -> Ptr CVecLit -> IO ()

foreign import ccall unsafe "minisat_addUnit"
    c_solver_addUnit :: CVar -> CInt -> Ptr CSolver -> IO ()

foreign import ccall unsafe "minisat_addBinary"
    c_solver_addBinary :: CVar -> CInt -> CVar -> CInt 
                       -> Ptr CSolver -> IO ()

foreign import ccall unsafe "minisat_addTernary"
    c_solver_addTernary :: CVar -> CInt -> CVar -> CInt -> CVar -> CInt 
                        -> Ptr CSolver -> IO ()

foreign import ccall safe "minisat_solve"
    c_solver_solve :: Ptr CSolver -> IO ()

foreign import ccall unsafe "minisat_okay"
    c_solver_okay :: Ptr CSolver -> IO Bool

-----------------------------------------------------------------------

foreign import ccall unsafe "minisat_newVecLit"
    c_vecLit_new :: IO (Ptr CVecLit)

foreign import ccall unsafe "minisat_deleteVecLit"
    c_vecLit_delete :: Ptr CVecLit -> IO ()

foreign import ccall unsafe "minisat_vecLit_pushVar"
    c_vecLit_pushVar :: Ptr CVecLit -> CVar -> CInt -> IO ()

foreign import ccall unsafe "minisat_vecLit_size"
    c_vecLit_size :: Ptr CVecLit -> CInt

foreign import ccall unsafe "minisat_vecLit_data"
    c_vecLit_data :: Ptr CVecLit -> Ptr CLit




