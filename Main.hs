import Data.Word
import MiniSat

import Criterion.Main
import Control.DeepSeq
import System.Environment

import Control.Monad.Trans.State.Strict

-- | A propositional logic formula
data Formula = And Formula Formula
             | Or Formula Formula
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


main = do
    args <- getArgs
    let n = read $ head args
        f = generateFormula n
        m = f `seq` maxVar f
        f' = m `seq` tseitin f m
    print $ f' `deepseq` length f'

instance NFData Literal where
    rnf (Pos a) = a `seq` ()
    rnf (Neg a) = a `seq` ()

--main = defaultMain [
--        bgroup "tseitin" [ bench "10" $ whnf tseitin (generateFormula 10)
--                         , bench "50" $ whnf tseitin (generateFormula 20)
--                         , bench "100" $ whnf tseitin (generateFormula 30)
--                         ]
--                   ]


--main = do
--    solver <- newSolver
--    x <- newVar solver
--    y <- newVar solver
--    --let n = nVars solver
--    n <- nVars solver
--    print (show x ++ " " ++ show y ++ " " ++ show n)






------------------------------------------------------------------------------

--newtype CNF = CNF [Clause] deriving (Show)
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

