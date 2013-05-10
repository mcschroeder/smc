import Data.Word
import MiniSat

data Formula = And Formula Formula 
             | Or Formula Formula
             | Not Formula 
             | Atom Var
             deriving (Show, Read)

f0 = Or (Atom 0) (Atom 0)
f1 = Or (Atom 0) (Atom 1)
f2 = And (Atom 0) (Atom 1)
f3 = Or (And (Atom 0) (Atom 1)) (And (Atom 2) (Atom 3))
f4 = Or (And (Atom 0) (Not (Atom 1))) (Atom 2)

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
    solver <- newSolver
    x <- newVar solver
    y <- newVar solver
    --let n = nVars solver
    n <- nVars solver
    print (show x ++ " " ++ show y ++ " " ++ show n)




------------------------------------------------------------------------------

newtype CNF = CNF [Clause] deriving (Show)

tseitin :: Formula -> CNF
tseitin f = CNF (cnf ++ [[Pos x]])
    where
        (_, x, cnf) = go (maxVar f) f

        go :: Var -> Formula -> (Var, Var, [[Literal]])        
        
        go n (Atom x) = (n, x, [])
        
        go n (Not f) = (x, x, cnf ++ cnf1)
            where
                (n1, y, cnf1) = go n f1
                x   = n1+1
                cnf = [[Neg x, Neg y], [Pos y, Pos x]]

        go n (Or f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
            where
                (n1, z, cnf1) = go n  f1
                (n2, y, cnf2) = go n1 f2
                x   = n2+1
                cnf = [[Neg y, Pos x], 
                       [Neg z, Pos x],
                       [Neg x, Pos y, Pos z]]

        go n (And f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
            where
                (n1, z, cnf1) = go n  f1
                (n2, y, cnf2) = go n1 f2
                x   = n2+1
                cnf = [[Neg x, Pos y],
                       [Neg x, Pos z],
                       [Neg y, Neg z, Pos x]]

maxVar :: Formula -> Var
maxVar = go 0
    where
        go n (Atom x)    = max n x
        go n (Not f)     = max n (go n f)
        go n (Or f1 f2)  = max (go n f1) (go n f2)
        go n (And f1 f2) = max (go n f1) (go n f2)

