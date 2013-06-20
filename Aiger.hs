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
unwind k Aiger{..} = [[Pos 0]] ++ cnf0 ++ [[Neg p0, output]] ++ go 1 k p0
    where
        p0   = maxVar + 1
        cnf0 = concatMap gateToCNF $ nextGates 0 constants [] gates

        output    = head outputs
        constants = map (\(s0,s1) -> (var s0, Neg 0)) latches
        flips     = map (\(s0,s1) -> (var s0, s1)) latches
        flops     = map (\(s0,s1) -> (var s1, s0)) latches

        flipflop k = if odd k then flips else flops

        go :: Int -> Int -> Var -> [Clause]
        go _ 0 p0 = [[Pos p0]]
        go k n p0 = [[Neg p0]] ++ cnf ++ [[Neg p, o]] ++ go (k+1) (n-1) p
            where
                minVar = p0
                p      = minVar + maxVar + 1
                o      = raiseIndex minVar output
                cnf    = concatMap gateToCNF $ 
                         nextGates minVar constants (flipflop k) gates

type VarMapping = [(Var,Literal)]

nextGates :: Var -> VarMapping -> VarMapping -> [Gate] -> [Gate]
nextGates minVar constants flipflop =
    (map . mapTuple3) (raiseIndex minVar . step constants . step flipflop)

step :: VarMapping -> Literal -> Literal
step assocs x0 = case lookup (var x0) assocs of
    Just x1 -> if isNeg x0 then neg x1 else x1
    Nothing -> x0

raiseIndex :: Var -> Literal -> Literal
raiseIndex = mapLit . incVar

incVar :: Var -> Var -> Var
incVar _ 0 = 0
incVar n v = v + n

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
