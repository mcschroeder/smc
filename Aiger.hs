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

unwind :: Int -> Aiger -> [Clause]
unwind k Aiger {..} = [Pos 0] : concatMap gateToCNF gates0 
                      ++ go 1 maxVar k [prop0]
    where
        go :: Int -> Var -> Int -> [Literal] -> [Clause]
        go _ _ 0 props = [props]
        go k m n props = concatMap gateToCNF (gatesk k m) ++
                         go (k + 1) (m + maxVar) (n - 1) (propk m : props)

        prop0 :: Literal
        prop0 = neg . head $ outputs

        propk :: Var -> Literal
        propk m = mapLit (+ m) prop0

        gates0 :: [Gate]
        gates0 = map (mapTuple3 (step constants)) gates

        gatesk :: Int -> Var -> [Gate]
        gatesk k m = map (mapTuple3 (mapLit (incVar m))) $ 
                     map (mapTuple3 (step constants)) $ 
                     map (mapTuple3 (step (flipflop k))) $ gates

        constants :: [(Var,Literal)]
        constants = map (\(s0,s1) -> (var s0, Neg 0)) latches

        flipflop :: Int -> [(Var,Literal)]
        flipflop k | odd k     = map (\(s0,s1) -> (var s0, s1)) latches
                   | otherwise = map (\(s0,s1) -> (var s1, s0)) latches

        step :: [(Var,Literal)] -> Literal -> Literal
        step assocs x0 = case lookup (var x0) assocs of
            Just x1 -> if isNeg x0 then neg x1 else x1
            Nothing -> x0

        incVar :: Var -> Var -> Var
        incVar m 0 = 0  -- we don't want to increase constants
        incVar m x = x + m


gateToCNF :: Gate -> [Clause]
gateToCNF (o,l0,l1) = [[neg o, l0], [neg o, l1], [o, neg l0, neg l1]]

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
