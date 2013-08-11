module Interpolation
    ( Proof
    , emptyProof
    , mkProofLogger
    , extractInterpolant

    , Label(..)
    ) where

import Control.Applicative
import Data.IntSet (IntSet, member, fromList)
import Data.List
import Data.Maybe

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

newtype Proof = Proof (DynamicVector Vertex)
data Vertex = Vertex [(Literal,Label)] Formula deriving (Show)

emptyProof :: IO Proof
emptyProof = Proof <$> V.new 100 -- TODO: tweak

-- TODO: abstract away Label
mkProofLogger :: Proof -> [Clause] -> [Clause] -> Label -> ProofLogger
mkProofLogger (Proof p) a b s = ProofLogger root chain deleted
    where
        a' = mkLitSet a
        b' = mkLitSet b

        label :: Literal -> Label
        label t = let t' = (fromIntegral . encodeLit) t
                  in if t' `member` a'
                         then if t' `member` b'
                                  then s
                                  else A
                         else B

        -- TODO: elem is O(n); would a hashmap pay off?
        aLocal :: Clause -> Bool
        aLocal = flip elem (map sort a) . sort

        root :: Clause -> IO ()
        root c = do
            let v = initialize label aLocal c
            V.append p v
            --putStrLn $ "root " ++ show c ++ "\t" ++ show v

        chain :: [ClauseId] -> [Var] -> IO ()
        chain cids vars =
            let go :: [Vertex] -> [Var] -> Vertex
                go [v3] _ = v3
                go (v1:v2:vs) (x:xs) = go (v:vs) xs
                    where v = resolve v1 v2 x

            in do
                vs <- mapM (V.unsafeRead p . fromIntegral) cids
                let v = go vs vars
                V.append p v
                --putStr   $ "chain " ++ show cids ++ " " ++ show vars
                --putStrLn $ "\t" ++ show v

        deleted :: ClauseId -> IO ()
        deleted cid = do
            V.unsafeWrite p (fromIntegral cid) undefined
            --putStrLn $ "deleted " ++ show cid


initialize :: (Literal -> Label) -> (Clause -> Bool) -> Clause -> Vertex
initialize label aLocal c = Vertex c1 i1
    where
        c1 = map (\t -> (t, label t)) c
        i1 | aLocal c  =       Or [Lit t | (t,l) <- c1, l .<=. B]
           | otherwise = Not $ Or [Lit t | (t,l) <- c1, l .<=. A]


resolve :: Vertex -> Vertex -> Var -> Vertex
resolve (Vertex c1 i1) (Vertex c2 i2) x = Vertex c3 i3
    where
        c3 = filter (\(t,_) -> var t /= x) (c1 ++ c2)
        i3 = case l_pos `join` l_neg of
            A  -> Or [i_pos, i_neg]
            AB -> And [ Or [Lit (Pos x), i_pos], Or [Lit (Neg x), i_neg] ]
            B  -> And [i_pos, i_neg]

        -- TODO: is this necessary, or are the inputs ordered (v+,v-) anyway?
        (i_pos, l_pos, i_neg, l_neg) = case lookup (Pos x) c1 of
            Just l1 -> ( i1, l1
                       , i2, fromJust $ lookup (Neg x) c2 )
            Nothing -> ( i2, fromJust $ lookup (Pos x) c2
                       , i1, fromJust $ lookup (Neg x) c1 )

mkLitSet :: [Clause] -> IntSet
mkLitSet = fromList . map (fromIntegral . encodeLit) . concat

extractInterpolant :: Proof -> IO Formula
extractInterpolant (Proof p) = do
    Vertex _ i <- V.unsafeRead p =<< subtract 1 <$> V.length p
    return i
