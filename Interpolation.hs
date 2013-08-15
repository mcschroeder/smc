{-# LANGUAGE RecordWildCards #-}

module Interpolation
    ( Interpolation, System(..)
    , newInterpolation, extractInterpolant
    , mkProofLogger
    ) where

import Control.Applicative
import Data.IntSet (IntSet, notMember, fromList)
import Data.List hiding (and,or)
import Data.Maybe
import Prelude hiding (and,or)

import DynamicVector (DynamicVector)
import qualified DynamicVector as V

import MiniSat
import Formula

------------------------------------------------------------------------------

-- a lattice with the partial ordering '.<=.' and 'join'
data Label = A | B | AB deriving (Eq, Show)

(.<=.) :: Label -> Label -> Bool
A  .<=. A  = True
A  .<=. AB = True
B  .<=. B  = True
B  .<=. AB = True
AB .<=. AB = True
_  .<=. _  = False

join :: Label -> Label -> Label
A `join` A = A
B `join` B = B
_ `join` _ = AB

------------------------------------------------------------------------------

data Interpolation = Interpolation {
    proof  :: DynamicVector Vertex,
    aLocal :: Clause -> Bool,
    label  :: Literal -> Label
}

data Vertex = Vertex [(Literal,Label)] Formula deriving (Show)

data System = McMillan | Symmetric | InverseMcMillan deriving (Show, Read)

newInterpolation :: [Clause] -> [Clause] -> System -> IO Interpolation
newInterpolation a b sys = do
    proof <- V.new 100 -- TODO: tweak
    let aLocal = flip elem (map sort a) . sort
    let a' = mkLitSet a
        b' = mkLitSet b
        label = stdLabel a' b' $ case sys of
            McMillan        -> B
            Symmetric       -> AB
            InverseMcMillan -> A

    return Interpolation{..}

mkLitSet :: [Clause] -> IntSet
mkLitSet = fromList . map (fromIntegral . encodeLit) . concat

stdLabel :: IntSet -> IntSet -> Label -> Literal -> Label
stdLabel a b s t =
    let t' = (fromIntegral . encodeLit) t in
    if t' `notMember` a
        then B
        else if t' `notMember` b
            then A
            else s

extractInterpolant :: Interpolation -> IO Formula
extractInterpolant Interpolation{..} = do
    Vertex [] i <- V.unsafeRead proof =<< subtract 1 <$> V.length proof
    return i

-----------------------------------------------------------------------

mkProofLogger :: Interpolation -> ProofLogger
mkProofLogger Interpolation{..} = ProofLogger root chain deleted
    where
        root :: Clause -> IO ()
        root c = V.append proof (initialize label aLocal c)

        chain :: [ClauseId] -> [Var] -> IO ()
        chain cids vars =
            let go :: [Vertex] -> [Var] -> Vertex
                go [v3] _ = v3
                go (v1:v2:vs) (x:xs) = go (v:vs) xs
                    where v = resolve v1 v2 x
            in do
                vs <- mapM (V.unsafeRead proof . fromIntegral) cids
                let v = go vs vars
                V.append proof v

        deleted :: ClauseId -> IO ()
        deleted cid = V.unsafeWrite proof (fromIntegral cid) undefined


initialize :: (Literal -> Label) -> (Clause -> Bool) -> Clause -> Vertex
initialize label aLocal c = Vertex c1 i1
    where
        c1 = map (\t -> (t, label t)) c
        i1 | aLocal c  =  ors [Lit t       | (t,l) <- c1, l .<=. B]
           | otherwise = ands [Lit (neg t) | (t,l) <- c1, l .<=. A]

        ors  = foldl or  (Lit (Pos 0))
        ands = foldl and (Lit (Neg 0))

resolve :: Vertex -> Vertex -> Var -> Vertex
resolve (Vertex c1 i1) (Vertex c2 i2) x = Vertex c3 i3'
    where
        c3 = filter (\(t,_) -> var t /= x) (c1 ++ c2)
        i3 = case l_pos `join` l_neg of
            A  -> i_pos `or` i_neg
            AB -> (Lit (Pos x) `or` i_pos) `and` (Lit (Neg x) `or` i_neg)
            B  -> i_pos `and` i_neg

        i3' = nubFormula i3
            where
                nubFormula (And xs) = And (nub xs)
                nubFormula (Or  xs) = Or  (nub xs)
                nubFormula x        = x

        -- MiniSat does not guarantee the ordering of the resolvents
        (i_pos, l_pos, i_neg, l_neg) = case lookup (Pos x) c1 of
            Just l1 -> ( i1, l1
                       , i2, fromJust $ lookup (Neg x) c2 )
            Nothing -> ( i2, fromJust $ lookup (Pos x) c2
                       , i1, fromJust $ lookup (Neg x) c1 )
