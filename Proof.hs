module Proof
    ( ProofNode
    , root
    , chain
    , deleted
    ) where

import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import MiniSat

data ProofNode = Root Clause
               | Chain Clause [ClauseId] [Var]
               | Sink [ClauseId]
               deriving (Show)


--solveWithProof :: Solver (Vector ProofNode)
--solveWithProof = do
--    proof <- liftIO $ VM.new 0
--    enableProofLogging (root proof) (chain proof) (deleted proof)
--    solve
--    liftIO $ V.freeze proof

root :: IOVector ProofNode -> Clause -> IO ()
root proof c = do
    putStrLn ("root: " ++ show c)
    --VM.grow proof 1
    --let i = (VM.length proof)-1
    --VM.write proof i (Root c)

chain :: IOVector ProofNode -> [ClauseId] -> [Var] -> IO ()
chain proof cs xs = do    
    let res = [] -- TODO
    putStrLn ("chain: cs=" ++ show cs ++ " xs=" ++ show xs ++ " res=TODO")
    --VM.grow proof 1
    --let i = (VM.length proof)-1
    --VM.write proof i (Chain res cs xs)

deleted :: IOVector ProofNode -> ClauseId -> IO ()
deleted proof c = do
    putStrLn ("deleted: " ++ show c)

