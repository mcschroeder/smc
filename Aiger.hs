{-# LANGUAGE RecordWildCards #-}

module Aiger 
    ( Aiger(..)
    , Latch
    , Gate
    , parseAiger
    , unwind
    ) where

import Control.Applicative ((<$>),(<*>))
import Text.ParserCombinators.Parsec

import Formula
import MiniSat

-----------------------------------------------------------------------

type Latch = (Literal, Literal)
type Gate = (Literal, Literal, Literal)

data Aiger = Aiger { maxVar  :: Var 
                   , inputs  :: [Literal]
                   , latches :: [Latch]
                   , outputs :: [Literal]
                   , gates   :: [Gate]
                   } deriving (Show)

-----------------------------------------------------------------------

-- [Note: True/False constants]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We assume there are no constants (literals -0 and 0, for true
-- and false, respectively) in the AIG, otherwise the unwinding
-- will be incorrect. (The unwound CNF will of course contain 
-- constants, arising from the initial states of the latches.)
-- We could assure this precondition by doing some kind of
-- reencoding after parsing, but this is currently not done.

unwind :: Aiger -> Int -> [Clause]
unwind Aiger{..} k = flip (++) [[rename k n (Pos p)]]
                   $ concat 
                   $ map ($ cnf) 
                   $ map (transition latches n) [0..k]
    where
        cnf = [Neg 0] : concatMap gateToCNF gates ++ [[Neg p, head outputs]]
        p   = Var $ fromIntegral n
        n   = fromIntegral (maxVar + 1)

transition :: [Latch] -> Int -> Int -> [Clause] -> [Clause]
transition ls n k = (map . map) (state ls n k)

state :: [Latch] -> Int -> Int -> Literal -> Literal
state ls _ 0 x = maybe x (const falseLit) (lookup x ls)
state ls n k x = case lookup x ls of
    Nothing -> rename k n x
    Just y  -> rename (k-1) n $ if isNeg x then neg y else y

falseLit = Pos 0
trueLit  = Neg 0

rename :: Int -> Int -> Literal -> Literal
rename k n = mapLit (fromIntegral (k * n) +)

gateToCNF :: Gate -> [Clause]
gateToCNF (o,l0,l1) = [[neg o, l0], [neg o, l1], [o, neg l0, neg l1]]

-----------------------------------------------------------------------

parseAiger :: FilePath -> IO (Either ParseError Aiger)
parseAiger = parseFromFile aiger

aiger :: Parser Aiger
aiger = do
    [m,i,l,o,a] <- header
    Aiger (fromIntegral m) 
      <$> rows i literal
      <*> rows l latch
      <*> rows o literal
      <*> rows a gate

header :: Parser [Int]
header = string "aag" >> count 5 (space >> integer)

rows :: Int -> Parser a -> Parser [a]
rows n p = count n (newline >> p)

latch :: Parser Latch
latch = (,) <$> literal <*> (space >> literal)

gate :: Parser Gate
gate = (,,) <$> literal <*> (space >> literal) <*> (space >> literal)

literal :: Parser Literal
literal = do
    i <- integer
    let v = Var $ fromIntegral $ i `div` 2
    if even i
        then return (Pos v)
        else return (Neg v)

integer :: Parser Int
integer = read <$> many1 digit
