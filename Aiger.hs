{-# LANGUAGE RecordWildCards #-}

module Aiger
    ( Aiger(..)
    , Latch
    , Gate
    , parseAiger
    , unwind
    , unwind'
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad
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


cnf2 (a,b) = [[neg a, b], [neg b, a]]
cnf3 (a,b,c) = [[neg a, b], [neg a, c], [a, neg b, neg c]]

--aigerToCNF :: Aiger -> ([Clause],[Clause],Var)
--aigerToCNF Aiger{..} = (q,t,n)
--    where
--        q = [Neg 0] : concatMap (\(v,_) -> cnf2 (Pos v, Pos 0)) latches
--        t = concatMap cnf3 gates ++ [[Neg n, head outputs]]
--        n = maxVar + 1

--forward :: Var -> [Clause] -> [Clause]
--forward = map . map . mapLit . (+)


----transition :: Aiger -> [Clause] -> [Clause] -> [Clause]
----transition Aiger{..} a ([Pos n]:b) = [Pos n'] : a' ++ b
----    where
----        a' = (map . map) (mapLit (+n)) a
----        n' = n + maxVar + 1



unwind' :: Aiger -> Int -> ([Clause],[Clause],[Clause])
unwind' Aiger{..} k = (q, t, p)
    where
        q = concatMap (latchToCNF k) latches
        t = concatMap (gateToCNF k) gates
        p = (map . map) (rename k) [[Pos n], [Neg n, head outputs]]
        n = maxVar + 1

        latchToCNF :: Int -> Latch -> [Clause]
        latchToCNF 0 (v,_) = [[Neg v, Pos 0], [Neg 0, Pos v]]
        latchToCNF k (v,t) = [[neg a, b], [neg b, a]]
            where
                a = rename k (Pos v)
                b = rename (k-1) t

        gateToCNF :: Int -> Gate -> [Clause]
        gateToCNF k (o,l0,l1) = [[neg a, b], [neg a, c], [a, neg b, neg c]]
            where
                a = rename k o
                b = rename k l0
                c = rename k l1

        rename :: Int -> Literal -> Literal
        rename = mapLit . (+) . ((maxVar + 1) *) . fromIntegral


unwind :: Aiger -> Int -> [Clause]
unwind Aiger{..} k = concatMap cnf [0..k] ++ [[rename k (Pos (maxVar + 1))]]
    where
        cnf k = [[rename k (Neg 0)]]
             ++ concatMap (latchToCNF k) latches
             ++ concatMap (gateToCNF k) gates
             ++ [map (rename k) [Neg (maxVar + 1), head outputs]]

        latchToCNF :: Int -> Latch -> [Clause]
        latchToCNF 0 (v,_) = [[Neg v, Pos 0], [Neg 0, Pos v]]
        latchToCNF k (v,t) = [[neg a, b], [neg b, a]]
            where
                a = rename k (Pos v)
                b = rename (k-1) t

        gateToCNF :: Int -> Gate -> [Clause]
        gateToCNF k (o,l0,l1) = [[neg a, b], [neg a, c], [a, neg b, neg c]]
            where
                a = rename k o
                b = rename k l0
                c = rename k l1

        rename :: Int -> Literal -> Literal
        rename = mapLit . (+) . ((maxVar + 1) *) . fromIntegral


simple = either undefined return =<< parseAiger "simple.aag"
simple_err = either undefined return =<< parseAiger "simple_err.aag"

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
