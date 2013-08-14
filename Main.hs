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
        q = fromCNF $ [[Neg 0]] ++ t0
        t = fromCNF t1
        b = fromCNF $ p1 ++ (map . map) neg p0

    let next :: Int -> Formula -> Formula
        next k (And xs) = And (ys ++ xs')
            where
                (t,p) = unwind aag k
                And ys = fromCNF (p ++ t)
                Or [Lit p0] = head xs
                xs' = Or [Lit (neg p0)] : tail xs

    let rewind :: Formula -> Formula
        rewind = mapFormula (mapLit mod')
            where
                mod' 0 = 0
                mod' v | r == 0    = n
                       | otherwise = r
                    where r = v `mod` n
                          n = Aiger.maxVar aag

    interpolate sys q (fromCNF p0) >>= \case
        Satisfiable     -> return False  -- "property trivially true" ???
        Unsatisfiable _ -> check sys 0 q t b next rewind

-- TODO: find a way to reuse solver instances & just incrementally add clauses

check :: System -> Int -> Formula -> Formula -> Formula
      -> (Int -> Formula -> Formula)  -- a function computing the next b
      -> (Formula -> Formula)  -- a function rewinding to 0
      -> IO Bool
check sys k q0 t0 b next rewind = do
    printf "check k=%d\n" k
    --printf "check3 k=%d q0=%s t0=%s b=%s\n" k (show q0) (show t0) (show b)
    let a = q0 `and` t0
    --printf "\ta = %s\n" (show a)
    interpolate sys a b >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return False
        Unsatisfiable i -> do
            --printf "\tUNSAT\n"
            printf "\tUNSAT i = %s\n" (show i)
            check1 <- implies a i
            check2 <- implies b (Not i)
            printf "\t SANITY CHECK: %s %s\n" (show check1) (show check2)
            let i' = rewind i
            --printf "\trewind i' = %s\n" (show i')
            let q0' = i' `or` q0
            --printf "\tq0' = %s\n" (show q0')
            fix sys q0' t0 b rewind >>= \case
                Unsatisfiable _ -> do
                    printf "\tFOUND FIXPOINT\n"
                    return True
                Satisfiable -> do
                    let b' = next (k+1) b
                    check sys (k+1) q0 t0 b' next rewind

fix :: System -> Formula -> Formula -> Formula
    -> (Formula -> Formula)  -- a function rewinding to 0
    -> IO Result
fix sys q0 t0 b rewind = do
    printf "fix\n"
    --printf "fix q0=%s t0=%s b=%s\n" (show q0) (show t0) (show b)
    let a = q0 `and` t0
    --printf "\ta = %s\n" (show a)
    interpolate sys a b >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return Satisfiable
        Unsatisfiable i -> do
            --printf "\tUNSAT\n"
            printf "\tUNSAT i = %s\n" (show i)
            check1 <- implies a i
            check2 <- implies b (Not i)
            printf "\t SANITY CHECK: %s %s\n" (show check1) (show check2)
            let i' = rewind i
            --printf "\trewind i' = %s\n" (show i')
            let q0' = i' `or` q0
            --printf "\tq0' = %s\n" (show q0')
            q0' `implies` q0 >>= \case
                True -> do
                    printf "\tq0' => q0\n"
                    return (Unsatisfiable i')
                False -> do
                    fix sys q0' t0 b rewind

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
        --addUnit (Neg 0)  -- NOTE how -0 is added to simulate T
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
    --addUnit (Neg 0)  -- NOTE how -0 is added to simulate T
    mapM_ addClause f'
    solve
    isOkay
