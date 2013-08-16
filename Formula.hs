{-# LANGUAGE LambdaCase #-}

module Formula
    ( Formula(..)
    , and, ands, or, ors
    , mapFormula,foldrFormula
    , maxVar
    , CNF, fromCNF, toCNF, tseitin
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
             | Lit !Literal
             deriving (Eq)

and :: Formula -> Formula -> Formula
and x             (Lit (Neg 0)) = x
and x             (Lit (Pos 0)) = Lit (Pos 0)
and (Lit (Neg 0)) y             = y
and (Lit (Pos 0)) y             = Lit (Pos 0)
and (And xs)      (And ys)      = And (xs ++ ys)
and x             (And ys)      = And (x:ys)
and (And xs)      y             = And (y:xs)
and x             y             = And [x,y]

ands :: [Formula] -> Formula
ands []     = Lit (Neg 0)
ands (x:xs) = and x $ ands xs

or :: Formula -> Formula -> Formula
or x             (Lit (Neg 0)) = Lit (Neg 0)
or x             (Lit (Pos 0)) = x
or (Lit (Neg 0)) y             = Lit (Neg 0)
or (Lit (Pos 0)) y             = y
or (Or xs)       (Or ys)       = Or (xs ++ ys)
or x             (Or ys)       = Or (x:ys)
or (Or xs)       y             = Or (y:xs)
or x             y             = Or [x,y]

ors :: [Formula] -> Formula
ors []     = Lit (Pos 0)
ors (x:xs) = or x $ ors xs

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

-----------------------------------------------------------------------

type CNF = [Clause]

fromCNF :: CNF -> Formula
fromCNF = ands . map (ors . map Lit)

-- | Given a formula and the next free variable (i.e. the lowest variable
-- not occuring in the formula that can be used as an auxiliary variable
-- in the Tseitin encoding) returns the formula in CNF and the new next
-- free variable.
toCNF :: Formula -> Var -> (CNF, Var)
toCNF f n | isCNF f   = (easy f, n)
          | otherwise = ([x]:xs, n')
    where
        easy (And xs) = [[y | Lit y <- ys] | Or ys <- xs]
        ((x, xs), n') = runState (tseitin f) n

isCNF :: Formula -> Bool
isCNF (And xs) = all (\case { Or ys -> all isLit ys; _ -> False }) xs
isCNF _        = False

isLit :: Formula -> Bool
isLit (Lit _) = True
isLit _       = False

tseitin :: Formula -> State Var (Literal, CNF)
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
