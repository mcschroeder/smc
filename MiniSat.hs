-- Haskell bindings for MiniSat 1.14 (proof logging version)
-- based in part on https://github.com/msakai/haskell-minisat

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MiniSat 
    ( Solver
    , newSolver
    , newVar
    , nVars

    , Var(..)
    , Literal(..)
    , Clause
    ) where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C

newtype Solver = Solver (ForeignPtr CSolver)

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

newSolver :: IO Solver
newSolver = Solver <$> (newForeignPtr p_solver_delete =<< c_solver_new)

withSolver :: Solver -> (Ptr CSolver -> IO a) -> IO a
withSolver (Solver fp) = withForeignPtr fp

nVars :: Solver -> IO Int
nVars s = fromIntegral <$> flip withSolver c_solver_nVars s

newVar :: Solver -> IO Var
newVar = fmap Var . flip withSolver c_solver_newVar

-----------------------------------------------------------------------

data CSolver
type CVar = CInt

foreign import ccall unsafe "minisat_newSolver"
    c_solver_new :: IO (Ptr CSolver)

foreign import ccall unsafe "& minisat_deleteSolver" 
    p_solver_delete :: FinalizerPtr CSolver

foreign import ccall unsafe "minisat_newVar" 
    c_solver_newVar :: Ptr CSolver -> IO CVar

foreign import ccall unsafe "minisat_nVars" 
    c_solver_nVars :: Ptr CSolver -> IO CInt
