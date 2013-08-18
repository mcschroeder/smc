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

-----------------------------------------------------------------------

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
                True  -> putStrLn "\nOK"
                False -> putStrLn "\nFAIL"

-----------------------------------------------------------------------

checkAiger :: Aiger -> System -> IO Bool
checkAiger aag sys = do
    let (ls0,gs0,o0,n0) = unwind aag 0
        (ls1,gs1,o1,n1) = unwind aag 1

    let q0 = fromCNF $ ls0
        t0 = fromCNF $ ls1 ++ gs0

    let b0 = mkClauseSet gs0
        b1 = mkClauseSet gs1

    interpolate sys q0 b0 [o0] n0 >>= \case
        Satisfiable     -> return False  -- property trivially true
        Unsatisfiable _ -> check aag sys 1 q0 t0 b1 [o1] n1


check :: Aiger -> System -> Int -> Formula -> Formula -> ClauseSet -> Clause -> Var
      -> IO Bool
check aag sys k q0 t0 b p maxVar = do
    printf "check k=%d\n" k
    let a = q0 `and` t0
    interpolate sys a b p maxVar >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return False
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            let i' = mapFormula (rewind aag) i
            let q0' = i' `or` q0
            fix aag sys q0' t0 b p maxVar >>= \case
                Unsatisfiable _ -> do
                    printf "\tFOUND FIXPOINT\n"
                    return True
                Satisfiable -> do
                    let (ls,gs,o,n') = unwind aag (k+1)
                        b' = clauseSetUnionCNF b (gs ++ ls)
                    check aag sys (k+1) q0 t0 b' (o:p) n'


fix :: Aiger -> System -> Formula -> Formula -> ClauseSet -> Clause -> Var
    -> IO Result
fix aag sys q0 t0 b p maxVar = do
    printf "fix\n"
    let a = q0 `and` t0
    interpolate sys a b p maxVar >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return Satisfiable
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            let i' = mapFormula (rewind aag) i
            let q0' = i' `or` q0
            q0' `implies` q0 >>= \case
                True -> do
                    printf "\tq0' => q0\n"
                    return (Unsatisfiable i')
                False -> do
                    fix aag sys q0' t0 b p maxVar

-----------------------------------------------------------------------

data Result = Satisfiable | Unsatisfiable Formula deriving (Show)

interpolate :: System -> Formula -> ClauseSet -> Clause -> Var -> IO Result
interpolate sys a b p maxVar = do
    let (a', n1) = toCNF a (maxVar + 1)
        b' = clauseSetInsert p b
    i <- newInterpolation a' b' sys
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
