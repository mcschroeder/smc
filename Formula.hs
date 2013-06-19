module Formula 
    ( Formula(..)
    ) where

import Data.Foldable
import Text.Read (readPrec,parens)
import Text.ParserCombinators.ReadP (string,choice)
import Text.ParserCombinators.ReadPrec ((<++))
import qualified Text.ParserCombinators.ReadPrec as ReadPrec (lift)

import Prelude hiding (foldr)

data Formula a = And [Formula a] 
               | Or  [Formula a]
               | Not (Formula a)
               | Lit a

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
