import Data.Word
import MiniSat

import Criterion.Main
import Control.DeepSeq
import System.Environment

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

main = do
    args <- getArgs
    let n = read $ head args
        f = generateFormula n
        f' = f `seq` tseitin f
    print $ length f'


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

tseitin :: Formula -> CNF
tseitin f = (cnf ++ [[Pos x]])
    where
        (_, x, cnf) = let n = maxVar f in n `seq` go n f

        go :: Var -> Formula -> (Var, Var, [[Literal]])        
        
        go n (Atom x) = n `seq` (n, x, [])
        
        go n (Not f) = (x, x, cnf ++ cnf1)
            where
                (n1, y, cnf1) = go n f1
                x   = n1+1
                cnf = [[Neg x, Neg y], [Pos y, Pos x]]

        --go n (Or (Atom y) (Atom z)) = (x, x, cnf)
        --    where
        --        x   = n `seq` n+1
        --        cnf = x `seq` [[Neg y, Pos x], 
        --               [Neg z, Pos x],
        --               [Neg x, Pos y, Pos z]]

        --go n (Or f1 (Atom y)) = (x, x, cnf ++ cnf1)
        --    where
        --        (n1, z, cnf1) = n `seq` go n  f1
        --        x   = n1 `seq` z `seq` cnf1 `seq` n1+1
        --        cnf = x `seq` [[Neg y, Pos x], 
        --               [Neg z, Pos x],
        --               [Neg x, Pos y, Pos z]]        

        --go n (Or (Atom y) f1) = (x, x, cnf ++ cnf1)
        --    where
        --        (n1, z, cnf1) = n `seq` go n  f1
        --        x   = n1 `seq` z `seq` cnf1 `seq` n1+1
        --        cnf = x `seq` [[Neg y, Pos x], 
        --               [Neg z, Pos x],
        --               [Neg x, Pos y, Pos z]]        

        go n (Or f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
            where
                (n1, z, cnf1) = n `seq` go n  f1
                (n2, y, cnf2) = n1 `seq` go n1 f2
                x   = n2 `seq` n2+1
                cnf = z `seq` y `seq` x `seq` [[Neg y, Pos x], 
                       [Neg z, Pos x],
                       [Neg x, Pos y, Pos z]]

        --go n (And (Atom y) (Atom z)) = (x, x, cnf)
        --    where
        --        x   = n `seq` n+1
        --        cnf = x `seq` [[Neg x, Pos y],
        --               [Neg x, Pos z],
        --               [Neg y, Neg z, Pos x]]

        --go n (And (Atom y) f1) = (x, x, cnf ++ cnf1)
        --    where
        --        (n1, z, cnf1) = n `seq` go n  f1
        --        x   = n1 `seq` z `seq` cnf1 `seq` n1+1
        --        cnf = x `seq` [[Neg x, Pos y],
        --               [Neg x, Pos z],
        --               [Neg y, Neg z, Pos x]]

        --go n (And f1 (Atom y)) = (x, x, cnf ++ cnf1)
        --    where
        --        (n1, z, cnf1) = n `seq` go n  f1
        --        x   = n1 `seq` z `seq` cnf1 `seq` n1+1
        --        cnf = x `seq` [[Neg x, Pos y],
        --               [Neg x, Pos z],
        --               [Neg y, Neg z, Pos x]]

        go n (And f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
            where
                (n1, z, cnf1) = n `seq` go n  f1
                (n2, y, cnf2) = n1 `seq` go n1 f2
                x   = n2 `seq` n2+1
                cnf = z `seq` y `seq` x `seq` [[Neg x, Pos y],
                       [Neg x, Pos z],
                       [Neg y, Neg z, Pos x]]

maxVar :: Formula -> Var
maxVar = go 0
    where
        go n (Atom x)    = max n x
        go n (Not f)     = max n (go n f)
        go n (Or f1 f2)  = max (go n f1) (go n f2)
        go n (And f1 f2) = max (go n f1) (go n f2)

