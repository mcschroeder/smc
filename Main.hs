import Data.Word
import MiniSat

type Var = Word

data Formula = And Formula Formula 
             | Or Formula Formula
             | Neg Formula 
             | Atom Var
             deriving (Show, Read)

f0 = Or (Atom 0) (Atom 0)
f1 = Or (Atom 0) (Atom 1)
f2 = And (Atom 0) (Atom 1)
f3 = Or (And (Atom 0) (Atom 1)) (And (Atom 2) (Atom 3))
f4 = Or (And (Atom 0) (Neg (Atom 1))) (Atom 2)

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

data Literal = Lit Var | NegLit Var

instance Show Literal where
    show (Lit    n) =     show n
    show (NegLit n) = '-':show n


newtype CNF = CNF [[Literal]] deriving (Show)

tseitin :: Formula -> CNF
tseitin f = CNF (cnf ++ [[Lit x]])
    where
        (_, x, cnf) = go (maxVar f) f

        go :: Var -> Formula -> (Var, Var, [[Literal]])        
        
        go n (Atom x) = (n, x, [])
        
        go n (Neg f) = (x, x, cnf ++ cnf1)
            where
                (n1, y, cnf1) = go n f1
                x   = n1+1
                cnf = [[NegLit x, NegLit y], [Lit y, Lit x]]

        go n (Or f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
            where
                (n1, z, cnf1) = go n  f1
                (n2, y, cnf2) = go n1 f2
                x   = n2+1
                cnf = [[NegLit y, Lit x], 
                       [NegLit z, Lit x],
                       [NegLit x, Lit y, Lit z]]

        go n (And f1 f2) = (x, x, cnf ++ cnf1 ++ cnf2)
            where
                (n1, z, cnf1) = go n  f1
                (n2, y, cnf2) = go n1 f2
                x   = n2+1
                cnf = [[NegLit x, Lit y],
                       [NegLit x, Lit z],
                       [NegLit y, NegLit z, Lit x]]

maxVar :: Formula -> Var
maxVar = go 0
    where
        go n (Atom x)    = max n x
        go n (Neg f)     = max n (go n f)
        go n (Or f1 f2)  = max (go n f1) (go n f2)
        go n (And f1 f2) = max (go n f1) (go n f2)

