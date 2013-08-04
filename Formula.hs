module Formula
    ( Formula(..)
    , maxVar, tseitin
    ) where

import Control.Monad.Trans.State
import Data.Foldable hiding (concatMap)
import Text.Read (readPrec,parens)
import Text.ParserCombinators.ReadP (string,choice)
import Text.ParserCombinators.ReadPrec ((<++))
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (lift)

import Prelude hiding (foldr)

import MiniSat

data Formula a = And [Formula a]
               | Or  [Formula a]
               | Not (Formula a)
               | Lit a

-- TODO: without this unnecessary generality, we could drop the type paramater
instance Foldable Formula where
    foldr f z (Lit x)  = f x z
    foldr f z (Not x)  = foldr f z x
    foldr f z (Or  xs) = foldr (flip $ foldr f) z xs
    foldr f z (And xs) = foldr (flip $ foldr f) z xs

instance Show a => Show (Formula a) where
    show (Lit x)  = show x
    show (Not x)  = "¬(" ++ show x ++ ")"
    show (And xs) = '⋀' : show xs
    show (Or xs)  = '⋁' : show xs

instance Read a => Read (Formula a) where
    readPrec = parens $ do ReadPrec.lift $ choice [string "And", string "⋀"]
                           readPrec >>= return . And
                    <++ do ReadPrec.lift $ choice [string "Or", string "⋁"]
                           readPrec >>= return . Or
                    <++ do ReadPrec.lift $ choice [string "-(", string "¬("]
                           f <- readPrec
                           ReadPrec.lift $ string ")"
                           return $ Not f
                    <++ do ReadPrec.lift $ string "Not"
                           readPrec >>= return . Not
                    <++ do readPrec >>= return . Lit

maxVar :: Formula Literal -> Var
maxVar = foldr max' 0
    where
        max' (Pos a) b = max a b
        max' (Neg a) b = max a b

tseitin :: Formula Literal -> Var -> (Literal, [Clause])
tseitin f n = evalState (go f) n
    where
        go :: Formula Literal -> State Var (Literal, [Clause])
        go (Lit x) = return (x, [])
        go (Not f) = do
            x <- newVar
            (y,cnf1) <- go f
            let cnf2 = [[Neg x, neg y], [Pos x, y]]
            return (Pos x, cnf1 ++ cnf2)
        go (Or fs) = do
            x <- newVar
            res <- mapM go fs
            let ys = map fst res
                cnfs1 = concatMap snd res
                cnf2 = map (flip (:) [Pos x] . neg) ys
                cnf3 = [(Neg x) : ys]
            return (Pos x, cnfs1 ++ cnf2 ++ cnf3)
        go (And fs) = do
            x <- newVar
            res <- mapM go fs
            let ys = map fst res
                cnfs1 = concatMap snd res
                cnf2 = map (flip (:) [Neg x]) ys
                cnf3 = [(Pos x) : map neg ys]
            return (Pos x, cnfs1 ++ cnf2 ++ cnf3)

        newVar :: State Var Var
        newVar = do
            x <- get
            put (x+1)
            return x

f0 = Or [Lit (Pos 0), Lit (Pos 0)]
f1 = Or [Lit (Pos 0), Lit (Pos 1)]
f2 = And [Lit (Pos 0), Lit (Pos 1)]
f3 = Or [And [Lit (Pos 0), Lit (Pos 1)], And [Lit (Pos 2), Lit (Pos 3)]]
f4 = Or [And [Lit (Pos 0), Not (Lit (Pos 1))], Lit (Pos 2)]
f5 = Or [And [Lit (Pos 0), Lit (Neg 1)], Lit (Pos 2)]
