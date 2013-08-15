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
    printf "Interpolation system: %s\n" (show sys)
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
test = either undefined return =<< parseAiger "../aiger/abc/test.aag"
test2 = either undefined return =<< parseAiger "../aiger/abc/test2.aag"

-----------------------------------------------------------------------

checkAiger :: Aiger -> System -> IO Bool
checkAiger aag sys = do
    let (t0,p0) = unwind aag 0
        (t1,p1) = unwind aag 1
        q = fromCNF $ {- [[Neg 0]] ++ -} t0
        t = fromCNF t1

    let next :: Int -> CNF -> (CNF, Var)
        next k xs = (xs' ++ t, var p)
            where
                (t,p) = unwind aag k
                xs' = (p : head xs) : tail xs

    let rewind :: Formula -> Formula
        rewind = mapFormula (Aiger.rewind aag 1)

    interpolate sys q [[p0]] (var p0) >>= \case
        Satisfiable     -> return False  -- property trivially true
        Unsatisfiable _ -> check sys 1 q t [[p1]] (var p1) next rewind


check :: System -> Int -> Formula -> Formula -> CNF -> Var
      -> (Int -> CNF -> (CNF, Var))
      -> (Formula -> Formula)
      -> IO Bool
check sys k q t b maxVar next rewind = do
    printf "check k=%d\n" k
    --printf "check3 k=%d q=%s t=%s b=%s\n" k (show q) (show t) (show b)
    let a = q `and` t
    --printf "\ta = %s\n" (show a)
    interpolate sys a b maxVar >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return False
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            --printf "\tUNSAT i = %s\n" (show i)
            --check1 <- implies a i
            --check2 <- implies (fromCNF b) (Not i)
            --printf "\t SANITY CHECK: %s %s\n" (show check1) (show check2)
            let i' = rewind i
            let q' = i' `or` q
            --printf "\tq' = %s\n" (show q')
            fix sys q' t b maxVar rewind >>= \case
                Unsatisfiable _ -> do
                    printf "\tFOUND FIXPOINT\n"
                    return True
                Satisfiable -> do
                    let (b', maxVar') = next (k+1) b
                    check sys (k+1) q t b' maxVar' next rewind


fix :: System -> Formula -> Formula -> CNF -> Var
    -> (Formula -> Formula)
    -> IO Result
fix sys q t b maxVar rewind = do
    printf "fix\n"
    --printf "fix q=%s t=%s b=%s\n" (show q) (show t) (show b)
    let a = q `and` t
    --printf "\ta = %s\n" (show a)
    interpolate sys a b maxVar >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return Satisfiable
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            --printf "\tUNSAT i = %s\n" (show i)
            --check1 <- implies a i
            --check2 <- implies (fromCNF b) (Not i)
            --printf "\t SANITY CHECK: %s %s\n" (show check1) (show check2)
            let i' = rewind i
            let q' = i' `or` q
            --printf "\tq' = %s\n" (show q')
            q' `implies` q >>= \case
                True -> do
                    printf "\tq' => q\n"
                    return (Unsatisfiable i')
                False -> do
                    fix sys q' t b maxVar rewind

-----------------------------------------------------------------------

data Result = Satisfiable | Unsatisfiable Formula deriving (Show)

interpolate :: System -> Formula -> CNF -> Var -> IO Result
interpolate sys a b maxVar = do
    --printf "interpolate %s %s\n" (show a) (show b)
    let (a', n1) = toCNF a (maxVar + 1)
    i <- newInterpolation a' b sys
    ok <- runSolverWithProof (mkProofLogger i) $ do
        replicateM_ (fromIntegral n1) newVar
        addUnit (Neg 0)
        mapM_ addClause a'
        mapM_ addClause b
        solve
        isOkay
    if ok then return Satisfiable
          else Unsatisfiable <$> extractInterpolant i

implies :: Formula -> Formula -> IO Bool
implies q' q = not <$> sat (q' `and` Not q)

sat :: Formula -> IO Bool
sat f = runSolver $ do
    --liftIO $ printf "sat %s\n" (show f)
    let n = 1 + Formula.maxVar f  -- TODO: eliminate
        (f', n') = toCNF f n
    replicateM_ (fromIntegral n') newVar
    addUnit (Neg 0)
    mapM_ addClause f'
    solve
    isOkay
