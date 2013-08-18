{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad
import Data.List hiding (and,or)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import System.Environment
import Text.Printf

import Prelude hiding (and,or)

import MiniSat
import Formula
import Aiger
import Interpolation

------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let file = args !! 0
        sys = if length args > 1
                then read (args !! 1)
                else McMillan
    printf "Interpolation system: %s\n" (show sys)
    parseAiger file >>= \case
        Left  err -> print err
        Right aag -> do
            checkAiger aag sys >>= \case
                Fixpoint         -> putStrLn $ "OK"
                Counterexample k -> putStrLn $ "FAIL @ k=" ++ (show k)

------------------------------------------------------------------------------

data CheckResult = Fixpoint | Counterexample Int deriving (Show)

checkAiger :: Aiger -> System -> IO CheckResult
checkAiger aag sys = do
    let (ls0,gs0,o0,_) = unwind aag 0
    sat (fromCNF $ [o0] : gs0 ++ ls0) >>= \case
        True -> return (Counterexample 0)
        False -> do
            let (ls1,gs1,o1,n1) = unwind aag 1
                q0 = fromCNF $ ls0
                t1 = fromCNF $ ls1 ++ gs0
                b1 = mkClauseSet gs1
                bLits1 = mkLitSet gs1
            check aag sys q0 t1 b1 bLits1 [o1] n1 1

check :: Aiger -> System
      -> Formula -> Formula
      -> ClauseSet -> LitSet -> Clause -> Var
      -> Int
      -> IO CheckResult
check aag sys q0 t1 b bLits p n k = do
    let a = q0 `and` t1
    interpolate sys a b bLits p n >>= \case
        Satisfiable -> return (Counterexample k)
        Unsatisfiable i -> do
            let i' = mapFormula (rewind aag) i
                q0' = i' `or` q0
            fix aag sys q0' t1 b bLits p n >>= \case
                True -> return Fixpoint
                False -> do
                    let (ls,gs,o,n') = unwind aag (k+1)
                        b' = clauseSetUnionCNF b (gs ++ ls)
                        bLits' = litSetUnionCNF bLits (gs ++ ls)
                    check aag sys q0 t1 b' bLits (o:p) n' (k+1)

fix :: Aiger -> System
    -> Formula -> Formula
    -> ClauseSet -> LitSet -> Clause -> Var
    -> IO Bool
fix aag sys q t1 b bLits p n = do
    let a = q `and` t1
    interpolate sys a b bLits p n >>= \case
        Satisfiable -> return False
        Unsatisfiable i -> do
            let i' = mapFormula (rewind aag) i
            let q' = i' `or` q
            q' `implies` q >>= \case
                True -> return True
                False -> fix aag sys q' t1 b bLits p n

------------------------------------------------------------------------------

data InterpolationResult = Satisfiable | Unsatisfiable Formula deriving (Show)

interpolate :: System
            -> Formula
            -> ClauseSet -> LitSet -> Clause -> Var
            -> IO InterpolationResult
interpolate sys a b bLits p n = do
    let (a', n1) = toCNF a (n+1)
        aLits = mkLitSet a'
        b' = clauseSetInsert p b
        bLits' = litSetUnionCNF bLits [p]
    i <- newInterpolation aLits bLits' b' sys
    ok <- runSolverWithProof (mkProofLogger i) $ do
        replicateM_ (fromIntegral n1) newVar
        addUnit (Neg 0)
        mapM_ addClause a'
        forClauses_ b' addClause
        solve
        isOkay
    if ok then return Satisfiable
          else Unsatisfiable <$> extractInterpolant i

implies :: Formula -> Formula -> IO Bool
implies q' q = not <$> sat (q' `and` Not q)

sat :: Formula -> IO Bool
sat f = runSolver $ do
    let n = 1 + Formula.maxVar f  -- TODO: eliminate
        (f', n') = toCNF f n
    replicateM_ (fromIntegral n') newVar
    addUnit (Neg 0)
    mapM_ addClause f'
    solve
    isOkay
