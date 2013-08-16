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
fail2 = either undefined return =<< parseAiger "fail2.aag"
fail3 = either undefined return =<< parseAiger "fail3.aag"
fail4 = either undefined return =<< parseAiger "fail4.aag"
fail5 = either undefined return =<< parseAiger "fail5.aag"

-----------------------------------------------------------------------

checkAiger :: Aiger -> System -> IO Bool
checkAiger aag sys = do
    let (ls0,gs0,o0) = unwind aag 0
        (ls1,gs1,o1) = unwind aag 1

    printf "maxVar = %s\n" (show $ Aiger.maxVar aag)

    let q0 = fromCNF $ ls0 ++ gs0
        t0 = fromCNF $ ls1
        b  = [o1] : gs1

    interpolate sys q0 [[o0]] (var o0) >>= \case
        Satisfiable     -> return False  -- property trivially true
        Unsatisfiable _ -> check aag sys 1 q0 t0 b (var o1)


check :: Aiger -> System -> Int -> Formula -> Formula -> CNF -> Var
      -> IO Bool
check aag sys k q0 t0 b maxVar = do
    printf "check k=%d\n" k
    --printf "check3 k=%d q0=%s t0=%s b=%s\n" k (show q0) (show t0) (show b)
    let a = q0 `and` t0
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
            let i' = mapFormula (rewind aag 0) i
            --printf "\t rewind i' = %s\n" (show i')
            --check1 <- implies a i'
            --check2 <- implies (fromCNF b) (Not i')
            --printf "\t SANITY CHECK: %s %s\n" (show check1) (show check2)
            let q0' = i' `or` q0
            --printf "\tq0' = %s\n" (show q0')
            fix aag sys q0' t0 b maxVar >>= \case
                Unsatisfiable _ -> do
                    printf "\tFOUND FIXPOINT\n"
                    return True
                Satisfiable -> do
                    let (ls,gs,o) = unwind aag (k+1)
                        t0' = t0 `and` (fromCNF ls)
                        b' = (o : head b) : tail b ++ gs
                    check aag sys (k+1) q0 t0' b' (var o)


fix :: Aiger -> System -> Formula -> Formula -> CNF -> Var
    -> IO Result
fix aag sys q0 t0 b maxVar = do
    printf "fix\n"
    --printf "fix q0=%s t0=%s b=%s\n" (show q0) (show t0) (show b)
    let a = q0 `and` t0
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
            let i' = mapFormula (rewind aag 0) i
            --printf "\t rewind i' = %s\n" (show i')
            --check1 <- implies a i'
            --check2 <- implies (fromCNF b) (Not i')
            --printf "\t SANITY CHECK: %s %s\n" (show check1) (show check2)
            let q0' = i' `or` q0
            --printf "\tq0' = %s\n" (show q0')
            q0' `implies` q0 >>= \case
                True -> do
                    printf "\tq0' => q0\n"
                    return (Unsatisfiable i')
                False -> do
                    fix aag sys q0' t0 b maxVar

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

        --liftIO $ printf "interpolate a'=%s b=%s\n" (show a') (show b)
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
