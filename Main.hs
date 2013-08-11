{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad
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
    parseAiger file >>= \case
        Left  err -> print err
        Right aag -> do
            checkAiger aag sys >>= \case
                True  -> putStrLn "\nOK"
                False -> putStrLn "\nFAIL"

-- TODO: remove
simple_ok = either undefined return =<< parseAiger "simple_ok.aag"
simple_err = either undefined return =<< parseAiger "simple_err.aag"
ken_flash_1 = either undefined return =<< parseAiger "../aiger/tip-aig-20061215/ken.flash^01.C.aag"

-----------------------------------------------------------------------

checkAiger :: Aiger -> System -> IO Bool
checkAiger aag sys = do
    let (q0,t0,p0) = unwind aag 0
        q = fromCNF q0
        t = fromCNF t0
        b = fromCNF p0

    let next :: Int -> Formula -> Formula
        next k (And xs) = And (ys ++ xs')
            where
                (q,t,p) = unwind aag k
                And ys = fromCNF (p ++ t ++ q)
                Or [Lit (Pos n)] = head xs
                xs' = Or [Lit (Neg n)] : tail xs

    check sys 0 q t b next

check :: System -> Int -> Formula -> Formula -> Formula
      -> (Int -> Formula -> Formula)  -- a function computing the next b
      -> IO Bool
check sys k q0 t0 b next = do
    printf "check k=%d\n" k
    --printf "check3 k=%d q0=%s t0=%s b=%s\n" k (show q0) (show t0) (show b)
    let a = q0 `and` t0
    --printf "\ta = %s\n" (show a)
    interpolate sys a b >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return False
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            --printf "\tUNSAT i = %s\n" (show i)
            let q0' = i `or` q0
            --printf "\tq0' = %s\n" (show q0')
            fix sys q0' t0 b >>= \case
                Unsatisfiable _ -> do
                    printf "\tFOUND FIXPOINT\n"
                    return True
                Satisfiable -> do
                    let b' = next (k+1) b
                    check sys (k+1) q0 t0 b' next

fix :: System -> Formula -> Formula -> Formula -> IO Result
fix sys q0 t0 b = do
    printf "fix\n"
    --printf "fix q0=%s t0=%s b=%s\n" (show q0) (show t0) (show b)
    let a = q0 `and` t0
    --printf "\ta = %s\n" (show a)
    interpolate sys a b >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return Satisfiable
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            --printf "\tUNSAT i = %s\n" (show i)
            let q0' = i `or` q0
            --printf "\tq0' = %s\n" (show q0')
            q0' `implies` q0 >>= \case
                True -> do
                    printf "\tq0' => q0\n"
                    return (Unsatisfiable i)
                False -> do
                    fix sys q0' t0 b

-----------------------------------------------------------------------

data Result = Satisfiable
            | Unsatisfiable Formula
            deriving (Show)

interpolate :: System -> Formula -> Formula -> IO Result
interpolate sys a b = do
    --printf "interpolate %s %s\n" (show a) (show b)
    let n0 = max (Formula.maxVar a) (Formula.maxVar b)  -- TODO: eliminate
        (a', n1) = toCNF a n0
        (b', n2) = toCNF b n1

    i <- newInterpolation a' b' sys
    ok <- runSolverWithProof (mkProofLogger i) $ do
        replicateM_ (fromIntegral n2) newVar
        addUnit (Neg 0)  -- NOTE how -0 is added to simulate T
        mapM_ addClause a'
        mapM_ addClause b'
        solve
        isOkay
    if ok then return Satisfiable
          else Unsatisfiable <$> extractInterpolant i

implies :: Formula -> Formula -> IO Bool
implies q' q = not <$> sat (q' `and` Not q)

sat :: Formula -> IO Bool
sat f = runSolver $ do
    --liftIO $ printf "sat %s\n" (show f)
    let n = Formula.maxVar f  -- TODO: eliminate
        (f', n') = toCNF f n
    replicateM_ (fromIntegral n') newVar
    mapM_ addClause f'
    solve
    isOkay
