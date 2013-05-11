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
tseitin (Atom x) = [[Pos x]]
tseitin f        = [Pos x]:cnf
    where
        (x, cnf) = let n = maxVar f in n `seq` go n f

        go :: Var -> Formula -> (Var, CNF)
        
        go n (Or (Atom y) (Atom z)) = (x, cnf)
            where
                x   = n `seq` n+1
                cnf = x `seq` ors x y z
        
        go n (Or f1 (Atom z)) = (x, cnf ++ cnf1)
            where
                (y, cnf1) = n `seq` go n f1
                x         = y `seq` y+1
                cnf       = x `seq` ors x y z

        go n (Or (Atom y) f2) = (x, cnf ++ cnf2)
            where
                (z, cnf2) = n `seq` go n f2
                x         = z `seq` z+1
                cnf       = x `seq` ors x y z

        go n (Or f1 f2) = (x, cnf ++ cnf1 ++ cnf2)
            where
                (y, cnf1) = n `seq` go n f1
                (z, cnf2) = y `seq` go y f2
                x         = z `seq` z+1
                cnf       = x `seq` ors x y z

        go n (And (Atom y) (Atom z)) = (x, cnf)
            where
                x   = n `seq` n+1
                cnf = x `seq` ands x y z
        
        go n (And f1 (Atom z)) = (x, cnf ++ cnf1)
            where
                (y, cnf1) = n `seq` go n f1
                x         = y `seq` y+1
                cnf       = x `seq` ands x y z

        go n (And (Atom y) f2) = (x, cnf ++ cnf2)
            where
                (z, cnf2) = n `seq` go n f2
                x         = z `seq` z+1
                cnf       = x `seq` ands x y z

        go n (And f1 f2) = (x, cnf ++ cnf1 ++ cnf2)
            where
                (y, cnf1) = n `seq` go n f1
                (z, cnf2) = y `seq` go y f2
                x         = z `seq` z+1
                cnf       = x `seq` ands x y z

        ors  x y z = [[Neg y, Pos x], [Neg z, Pos x], [Neg x, Pos y, Pos z]]
        ands x y z = [[Neg x, Pos y], [Neg x, Pos z], [Neg y, Neg z, Pos x]]

        --go n (Or f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
        --    where
        --        (n1, z, cnf1) = n `seq` go n  f1
        --        (n2, y, cnf2) = n1 `seq` go n1 f2
        --        x   = n2 `seq` n2+1
        --        cnf = z `seq` y `seq` x `seq` [[Neg y, Pos x], 
        --               [Neg z, Pos x],
        --               [Neg x, Pos y, Pos z]]

        --go n (And f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
        --    where
        --        (n1, z, cnf1) = n `seq` go n  f1
        --        (n2, y, cnf2) = n1 `seq` go n1 f2
        --        x   = n2 `seq` n2+1
        --        cnf = z `seq` y `seq` x `seq` [[Neg x, Pos y],
        --               [Neg x, Pos z],
        --               [Neg y, Neg z, Pos x]]

maxVar :: Formula -> Var
maxVar = go 0
    where
        go n (Atom x)    = max n x
        go n (Not f)     = max n (go n f)
        go n (Or f1 f2)  = max (go n f1) (go n f2)
        go n (And f1 f2) = max (go n f1) (go n f2)

