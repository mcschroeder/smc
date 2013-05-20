-- Haskell bindings for MiniSat 1.14 (proof logging version)
-- based in part on https://github.com/msakai/haskell-minisat

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module MiniSat 
    ( Var(..)
    , Literal(..)
    , Clause

    , Solver
    , runSolver
    , newVar
    , nVars
    , addClause
    , solve
    , isOkay

      -- * Re-export
    , liftIO
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Foreign
import Foreign.C

import Control.Exception (bracket)

-----------------------------------------------------------------------

newtype Var = Var CVar deriving (Eq, Ord, Num, Enum, Read)

instance Show Var where
    show (Var n) = show n

data Literal = Pos {-# UNPACK #-} !Var
             | Neg {-# UNPACK #-} !Var
             deriving (Eq, Ord)

instance Show Literal where
    show (Pos n) =     show n
    show (Neg n) = '-':show n

type Clause = [Literal]

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

solve :: Solver ()
solve = withSolver c_solver_solve

isOkay :: Solver Bool
isOkay = withSolver c_solver_okay

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

foreign import ccall safe "minisat_solve"
    c_solver_solve :: Ptr CSolver -> IO ()

foreign import ccall unsafe "minisat_okay"
    c_solver_okay :: Ptr CSolver -> IO Bool

foreign import ccall unsafe "minisat_newVecLit"
    c_vecLit_new :: IO (Ptr CVecLit)

foreign import ccall unsafe "minisat_deleteVecLit"
    c_vecLit_delete :: Ptr CVecLit -> IO ()

foreign import ccall unsafe "minisat_vecLit_pushVar"
    c_vecLit_pushVar :: Ptr CVecLit -> CVar -> CInt -> IO ()
