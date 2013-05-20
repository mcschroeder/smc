import Data.Word
import MiniSat

import Criterion.Main
import Control.DeepSeq
import System.Environment

import Control.Monad
import Control.Monad.Trans.State.Strict

import Data.Sequence (Seq, (<|), (|>), (><))
import qualified Data.Sequence as Seq

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


--main = do
--    args <- getArgs
--    let n = read $ head args
--        f = generateFormula n
--        m = f `seq` maxVar f
--        f' = m `seq` tseitin f m
--    print $ f' `deepseq` Seq.length f'

--instance NFData Literal where
--    rnf (Pos a) = a `seq` ()
--    rnf (Neg a) = a `seq` ()

-- blergh
maxVarCNF :: CNF -> Var
maxVarCNF xs = case go (Pos 0) xs of
    Pos n -> n
    Neg n -> n
    where 
        go n [] = n
        go n (x:xs) = let y = maximum x 
                      in if y > n 
                        then go y xs 
                        else go n xs

main = do
    args <- getArgs
    let n = read $ head args
        f = generateFormula n
    --let f = (And (Atom 1) (Atom 1))
        m = f `seq` maxVar f        
    solver <- newSolver
    
    --addFormulaToSolver solver f m
        
    let cnf = m `seq` tseitin f m
        k = maxVarCNF cnf
    replicateM_ (fromEnum k) (newVar solver)
    mapM_ (addClause solver) cnf
    
    --print (show cnf)

    numvars <- nVars solver
    print (show numvars ++ " k=" ++ show k)

    solve solver
    ok <- okay solver
    print (show ok)

--addFormulaToSolver :: Solver -> Formula -> Var -> IO ()
--addFormulaToSolver solver f n = do
--    replicateM_ n (newVar solver)
--    x <- addWithTseitin solver f
--    addUnary solver x
--    where
--        addWithTseitin :: Solver -> Formula -> IO Var
--        addWithTseitin solver (Atom x) = return x
        
--        addWithTseitin solver (Or f1 f2) = do
--            x <- newVar solver
--            y <- x `seq` go f1
--            z <- y `seq` go f2
--            z `seq` do
--                addBinary solver (Neg y) (Pos x)
--                addBinary solver (Neg z) (Pos x)
--                addTernary solver (Neg x) (Pos y) (Pos z)
--            return x

--        addWithTseitin solver (And f1 f2) = do
--            x <- newVar solver
--            y <- x `seq` go f1
--            z <- y `seq` go f2
--            z `seq` do
--                addBinary solver (Neg x) (Pos y)
--                addBinary solver (Neg x) (Pos z)
--                addTernary solver (Neg y) (Neg z) (Pos x)
--            return x



------------------------------------------------------------------------------

type CNF = [Clause]

tseitin :: Formula -> Var -> CNF
tseitin f n = [Pos x]:cnf
    where
        (x, cnf) = evalState (go f) n

        go :: Formula -> State Var (Var, CNF)
        go (Atom x) = return (x, [])
        
        go (Or f1 f2) = do
            x <- newLabel
            (y, cnf1) <- x `seq` go f1
            (z, cnf2) <- y `seq` go f2
            return (x,   z `seq` orEnc x y z ++ cnf1 ++ cnf2)
        
        go (And f1 f2) = do
            x <- newLabel
            (y, cnf1) <- x `seq` go f1
            (z, cnf2) <- y `seq` go f2
            return (x,   z `seq` andEnc x y z ++ cnf1 ++ cnf2)

        orEnc  x y z = [[Neg y, Pos x], [Neg z, Pos x], [Neg x, Pos y, Pos z]]
        andEnc x y z = [[Neg x, Pos y], [Neg x, Pos z], [Neg y, Neg z, Pos x]]

        newLabel :: State Var Var
        newLabel = do
            x <- get
            let x' = x+1
            put x'
            return x'

