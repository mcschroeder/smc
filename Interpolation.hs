{-# LANGUAGE LambdaCase #-}

module Interpolation where

import Control.Applicative
import Data.IntSet (IntSet, member, fromList)
import qualified DynamicVector as V
import MiniSat
import Formula
import Proof

type LitSet = IntSet

mkLitSet :: [Clause] -> LitSet
mkLitSet = fromList . map (fromIntegral . encodeLit) . concat

data Locality = A | B | AB

locality :: LitSet -> LitSet -> Literal -> Locality
locality a b lit = let t = (fromIntegral . encodeLit) lit
                   in if t `member` a
                          then if t `member` b
                                   then AB
                                   else A
                          else B

data Color = Blue | Red | Purple

join :: Color -> Color -> Color
join Blue Blue = Blue
join Red  Red  = Red
join _    _    = Purple

data System = McMillan | Symmetric | InverseMcMillan

color :: System -> Locality -> Color
color McMillan        = \case { A -> Blue; AB -> Red;    B -> Red }
color Symmetric       = \case { A -> Blue; AB -> Purple; B -> Red }
color InverseMcMillan = \case { A -> Blue; AB -> Blue;   B -> Red }


interpolant :: System -> LitSet -> LitSet -> Proof -> IO (Formula Literal)
interpolant s a b p = go =<< V.unsafeRead p =<< subtract 1 <$> V.length p
    where
        res x = join (color s $ locality a b $ Pos x)
                     (color s $ locality a b $ Neg x)

        go (Root c) =
            -- NOTE: in the base case, all literals of the clause are in the same class,
            --       which is either A or B
            case color s $ locality a b $ head c of
                Blue -> return $ Lit (Neg 0) -- T
                Red  -> return $ Lit (Pos 0) -- F
                Purple -> error "initial vertex with shared literals"


        -- TODO: this is the same as sink
        go (Chain _ [cid1,cid2] [x]) = do
            i1 <- go =<< V.unsafeRead p (fromIntegral cid1)
            i2 <- go =<< V.unsafeRead p (fromIntegral cid2)
            case res x of
                Blue   -> return $ Or [i1,i2]
                Purple -> return $ And [Or [Lit (Pos x), i1], Or [Lit (Neg x), i2]]
                Red    -> return $ And [i1,i2]

        -- TODO: general case: (Sink [ClauseId] [Var])
        go (Sink [cid1,cid2] [x]) = do
            i1 <- go =<< V.unsafeRead p (fromIntegral cid1)
            i2 <- go =<< V.unsafeRead p (fromIntegral cid2)
            case res x of
                Blue   -> return $ Or [i1,i2]
                Purple -> return $ And [Or [Lit (Pos x), i1], Or [Lit (Neg x), i2]]
                Red    -> return $ And [i1,i2]


{-

two approaches:

1) bottom up, starting at the sink
        + we don't need to waste space on partial interpolants
        + use existing proof logger
        - no partial information -> harder to debug
        - need to be in IO because of vector?
        - only McMillan & Symmetric

2) top down, custom proof logger:
        + all information, better for debugging
        + (or only as much info as we need)
        + possibility for labelling functions that assign different colors
          to different occurences of the same literal
        - have to store all partial interpolants (more memory, but faster?)
          (maybe insignificant)

-}





