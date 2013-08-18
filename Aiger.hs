{-# LANGUAGE RecordWildCards #-}

module Aiger
    ( Aiger(..), Latch, Gate
    , parseAiger
    , unwind, rewind
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
import Data.Maybe
import Text.ParserCombinators.Parsec

import Formula
import MiniSat

-----------------------------------------------------------------------

type Latch = (Var, Literal)
type Gate = (Literal, Literal, Literal)

data Aiger = Aiger { maxVar  :: Var
                   , inputs  :: [Literal]
                   , latches :: [Latch]
                   , outputs :: [Literal]
                   , gates   :: [Gate]
                   } deriving (Show)

-----------------------------------------------------------------------

unwind :: Aiger -> Int -> ([Clause],[Clause],Literal,Var)
unwind Aiger{..} k = (ls,gs,o,n)
    where
        ls = concatMap (latchToCNF k) latches
        gs = concatMap (gateToCNF k) gates
        o = rename k $ head outputs
        n = maxVar * fromIntegral (k+1)

        latchToCNF :: Int -> Latch -> [Clause]
        latchToCNF 0 (v,_) = [[Neg v]]
        latchToCNF k (v,t) = [[neg a, b], [a, neg b]]
            where
                a = rename k (Pos v)
                b = rename (k-1) t

        gateToCNF :: Int -> Gate -> [Clause]
        gateToCNF k (x,y,z) = [[a, neg b, neg c], [neg a, b], [neg a, c]]
            where
                a = rename k x
                b = rename k y
                c = rename k z

        rename :: Int -> Literal -> Literal
        rename = mapLit . (+) . (maxVar *) . fromIntegral


rewind :: Aiger -> Literal -> Literal
rewind Aiger{..} = mapLit mod'
    where
        mod' 0 = 0
        mod' v | r == 0    = maxVar
               | otherwise = r
            where r = v `mod` maxVar

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
latch = (,) <$> variable <*> (space >> literal)

gate :: Parser Gate
gate = (,,) <$> literal <*> (space >> literal) <*> (space >> literal)

variable :: Parser Var
variable = Var . fromIntegral . flip div 2 <$> mfilter even integer

literal :: Parser Literal
literal = decodeLit . fromIntegral <$> integer

integer :: Parser Int
integer = read <$> many1 digit
