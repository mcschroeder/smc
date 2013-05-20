import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Word
import System.Environment

import Data.Sequence (Seq, (<|), (|>), (><))

import qualified Data.Sequence as Seq

import MiniSat


-- | A propositional logic formula
data Formula = And Formula Formula
             | Or  Formula Formula
             | Not Formula
             | Atom {-# UNPACK #-} !Var
             deriving (Show, Read)


-- some example formulas
f0 = Or (Atom 0) (Atom 0)
f1 = Or (Atom 0) (Atom 1)
f2 = And (Atom 0) (Atom 1)
f3 = Or (And (Atom 0) (Atom 1)) (And (Atom 2) (Atom 3))
f4 = Or (And (Atom 0) (Not (Atom 1))) (Atom 2)
f5 = And (And (Not (Atom 0)) (Or (Atom 0) (Not (Atom 1)))) 
         (Or (And (Atom 0) (Not (Atom 1))) (Atom 1))

-- generates formulas with alternating and / or connectives, for testing
generateFormula :: Word -> Formula
generateFormula = snd . go 0
    where
        go x 0 = (x+1, Atom x)
        go x n = let (x1,f1) = go x  (n-1)
                     (x2,f2) = go x1 (n-1)
                 in if even n
                    then (x2, Or f1 f2)
                    else (x2, And f1 f2)


maxVar :: Formula -> Var
maxVar = go 0
    where
        go n (Atom x)    = max n x
        go n (Not f)     = max n (go n f)
        go n (Or f1 f2)  = max (go n f1) (go n f2)
        go n (And f1 f2) = max (go n f1) (go n f2)


main = do
    args <- getArgs
    let n = read $ head args
        f = generateFormula n
    solveFormula f

solveFormula f = 
    runSolver $ do
        addFormula f
        liftIO . print =<< nVars
        liftIO . print =<< isOkay
        liftIO $ print "solving..."
        solve
        liftIO . print =<< isOkay


addFormula :: Formula -> Solver ()
addFormula f = do
    replicateM_ (fromEnum $ maxVar f) newVar   -- add atom vars
    x <- tseitin f
    addUnit (Pos x)
    where 
        tseitin :: Formula -> Solver Var        
        tseitin (Atom x) = return x
        
        tseitin (Not f) = do
            x <- newVar
            y <- tseitin f
            addBinary (Neg x) (Neg y)
            addBinary (Pos y) (Pos x)
            return x
        
        tseitin (Or f1 f2) = do
            x <- newVar
            y <- x `seq` tseitin f1
            z <- y `seq` tseitin f2
            addBinary  (Neg y) (Pos x)
            addBinary  (Neg z) (Pos x)
            addTernary (Neg x) (Pos y) (Pos z)
            return x
        
        tseitin (And f1 f2) = do
            x <- newVar
            y <- x `seq` tseitin f1
            z <- y `seq` tseitin f2
            addBinary  (Neg x) (Pos y)
            addBinary  (Neg x) (Pos z)
            addTernary (Neg y) (Neg z) (Pos x)
            return x
