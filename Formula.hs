module Formula
    ( Formula(..)
    , and, or
    , mapFormula,foldrFormula
    , maxVar
    , fromCNF, toCNF, tseitin
    ) where

import Control.Monad.Trans.State
import Prelude hiding (and,or)
import Text.Read (readPrec,parens)
import Text.ParserCombinators.ReadP (string,choice)
import Text.ParserCombinators.ReadPrec ((<++), lift)

import MiniSat

-----------------------------------------------------------------------

data Formula = And [Formula]
             | Or  [Formula]
             | Not Formula
             | Lit Literal
             deriving (Eq)

and :: Formula -> Formula -> Formula
and (And xs) (And ys) = And (xs ++ ys)
and (And xs) y        = And (y:xs)
and x        (And ys) = And (x:ys)
and x        y        = And [x,y]

or :: Formula -> Formula -> Formula
or (Or xs) (Or ys) = Or (xs ++ ys)
or (Or xs) y       = Or (y:xs)
or x       (Or ys) = Or (x:ys)
or x       y       = Or [x,y]

mapFormula :: (Literal -> Literal) -> Formula -> Formula
mapFormula f (And xs) = And $ map (mapFormula f) xs
mapFormula f (Or  xs) = Or  $ map (mapFormula f) xs
mapFormula f (Not x)  = Not $ mapFormula f x
mapFormula f (Lit x)  = Lit $ f x

foldrFormula :: (Literal -> b -> b) -> b -> Formula -> b
foldrFormula f z (Lit x)  = f x z
foldrFormula f z (Not x)  = foldrFormula f z x
foldrFormula f z (Or  xs) = foldr (flip $ foldrFormula f) z xs
foldrFormula f z (And xs) = foldr (flip $ foldrFormula f) z xs

maxVar :: Formula -> Var
maxVar = foldrFormula max' 0
    where
        max' (Pos a) b = max a b
        max' (Neg a) b = max a b

fromCNF :: [Clause] -> Formula
fromCNF = And . map (Or . map Lit)

-- | Given a formula and the next free variable (i.e. the lowest variable
-- not occuring in the formula that can be used as an auxiliary variable
-- in the Tseitin encoding) returns the formula in CNF and the new next
-- free variable.
toCNF :: Formula -> Var -> ([Clause], Var)
toCNF f n = ([x]:xs, n')
    where ((x, xs), n') = runState (tseitin f) n

tseitin :: Formula -> State Var (Literal, [Clause])
tseitin (Lit x) = return (x, [])
tseitin (Not f) = do
    x <- get
    put (x+1)
    (y,cnf1) <- tseitin f
    let cnf2 = [[Neg x, neg y], [Pos x, y]]
    return (Pos x, cnf1 ++ cnf2)
tseitin (Or fs) = do
    x <- get
    put (x+1)
    res <- mapM tseitin fs
    let ys = map fst res
        cnfs1 = concatMap snd res
        cnf2 = map (flip (:) [Pos x] . neg) ys
        cnf3 = [(Neg x) : ys]
    return (Pos x, cnfs1 ++ cnf2 ++ cnf3)
tseitin (And fs) = do
    x <- get
    put (x+1)
    res <- mapM tseitin fs
    let ys = map fst res
        cnfs1 = concatMap snd res
        cnf2 = map (flip (:) [Neg x]) ys
        cnf3 = [(Pos x) : map neg ys]
    return (Pos x, cnfs1 ++ cnf2 ++ cnf3)

-----------------------------------------------------------------------

instance Show Formula where
    show (Lit x)  = show x
    show (Not x)  = "¬(" ++ show x ++ ")"
    show (And xs) = '⋀' : show xs
    show (Or xs)  = '⋁' : show xs

instance Read Formula where
    readPrec = parens $ do lift $ choice [string "And", string "⋀"]
                           readPrec >>= return . And
                    <++ do lift $ choice [string "Or", string "⋁"]
                           readPrec >>= return . Or
                    <++ do lift $ choice [string "-(", string "¬("]
                           f <- readPrec
                           lift $ string ")"
                           return $ Not f
                    <++ do lift $ string "Not"
                           readPrec >>= return . Not
                    <++ do readPrec >>= return . Lit
