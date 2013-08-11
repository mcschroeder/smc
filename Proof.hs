{-# LANGUAGE LambdaCase #-}

module Proof
    ( Proof
    , ProofNode(..)
    , emptyProof
    , mkProofLogger
    ) where

import Control.Applicative
import DynamicVector (DynamicVector)
import qualified DynamicVector as V
import MiniSat

type Proof = DynamicVector ProofNode

data ProofNode = Root Clause
               | Chain Clause [ClauseId] [Var]
               | Sink [ClauseId] [Var]
               | Deleted
               deriving (Show)

emptyProof :: IO Proof
emptyProof = V.new 100

mkProofLogger :: Proof -> ProofLogger
mkProofLogger p = ProofLogger root chain deleted
    where
        root clause = V.append p (Root clause)

        chain clauseIds vars = do
            clauses <- mapM (readClause p) clauseIds
            let res = resolve clauses vars
            if null res
                then V.append p (Sink clauseIds vars)
                else V.append p (Chain res clauseIds vars)

        deleted clauseId = V.unsafeWrite p (fromIntegral clauseId) Deleted

readClause :: Proof -> ClauseId -> IO Clause
readClause p i = V.unsafeRead p (fromIntegral i) >>= \case
    Root  c     -> return c
    Chain c _ _ -> return c
    Sink    _ _ -> error "Proof.readClause: sink"
    Deleted     -> error "Proof.readClause: deleted"

resolve :: [Clause] -> [Var] -> Clause
resolve [c] [] = c
resolve (c:d:xs) (a:ys) = resolve (d':xs) ys
    where d' = filter ((/= a) . var) (c ++ d)
