{-# LANGUAGE RecordWildCards #-}

module Aiger 
    ( Aiger(..)
    , Latch
    , Gate
    , parseAiger
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

unwind0 :: Aiger -> [Clause]
unwind0 Aiger {..} = [Pos 0] : props : concatMap gateToCNF gates'
    where
        latches0 = map (\(s0,s1) -> (var s0, Neg 0)) latches
        gates' = replaceVars latches0 gates
        props = [neg . head $ outputs]


gateToCNF :: Gate -> [Clause]
gateToCNF (o,l0,l1) = [[neg o, l0], [neg o, l1], [o, neg l0, neg l1]]

replaceVars :: [(Var, Literal)] -> [Gate] -> [Gate]
replaceVars xs = map (mapTuple3 (rep xs))
    where
        rep :: [(Var, Literal)] -> Literal -> Literal
        rep xs y = case lookup (var y) xs of
            Just x  -> if isNeg y then neg x else x
            Nothing -> y

mapTuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mapTuple3 f (x,y,z) = (f x, f y, f z)

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
