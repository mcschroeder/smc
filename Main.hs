{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Foldable hiding (mapM_,sequence_)
import Data.Word
import System.Environment
import Text.Printf

import Prelude hiding (foldr,concat)

import MiniSat
import Formula
import Aiger
import Interpolation2

main :: IO ()
main = do
    args <- getArgs
    let file = args !! 0
    parseAiger file >>= \case
        Left err -> print err
        Right aag -> do
            printf "maxVar=%s\n" (show (Aiger.maxVar aag))
            checkAiger aag >>= \case
                True -> putStrLn "\nOK"
                False -> putStrLn "\nFAIL"

simple_ok = either undefined return =<< parseAiger "simple_ok.aag"
simple_err = either undefined return =<< parseAiger "simple_err.aag"

cnf2f :: [Clause] -> Formula Literal
cnf2f = simplify . And . map (Or . map Lit)

maxVarClause :: [Clause] -> Var
maxVarClause = var . foldr max (Pos 0) . concat


--------------------

checkAiger aag = do
    let (q0,t0,p0) = unwind' aag 0
        q = cnf2f q0
        t = cnf2f t0
        b = cnf2f p0
    check 0 q t b (next aag)

next aag k (And ((Or [Lit (Pos n)]):xs)) = And (ys ++ (Or [Lit (Neg n)]):xs)
    where
        (q,t,p) = unwind' aag k
        And ys = cnf2f (p ++ t ++ q)

check k q0 t0 b next = do
    printf "check k=%d\n" k
    --printf "check3 k=%d q0=%s t0=%s b=%s\n" k (show q0) (show t0) (show b)
    let a = simplify $ And [q0, t0]
    --printf "\ta = %s\n" (show a)
    interpolate a b >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return False
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            --printf "\tUNSAT i = %s\n" (show i)
            let q0' = simplify $ Or [i, q0]
            --printf "\tq0' = %s\n" (show q0')
            fix q0' t0 b >>= \case
                Unsatisfiable _ -> do
                    printf "\tFOUND FIXPOINT\n"
                    return True
                Satisfiable -> do
                    let b' = next (k+1) b
                    check (k+1) q0 t0 b' next

fix q0 t0 b = do
    printf "fix\n"
    --printf "fix q0=%s t0=%s b=%s\n" (show q0) (show t0) (show b)
    let a = simplify $ And [q0, t0]
    --printf "\ta = %s\n" (show a)
    interpolate a b >>= \case
        Satisfiable -> do
            printf "\tSAT\n"
            return Satisfiable
        Unsatisfiable i -> do
            printf "\tUNSAT\n"
            --printf "\tUNSAT i = %s\n" (show i)
            let q0' = simplify $ Or [i, q0]
            --printf "\tq0' = %s\n" (show q0')
            q0' `implies` q0 >>= \case
                True -> do
                    printf "\tq0' => q0\n"
                    return (Unsatisfiable i)
                False -> do
                    fix q0' t0 b




data Result = Satisfiable | Unsatisfiable (Formula Literal) deriving (Show)

-- TODO: choice of system
interpolate :: Formula Literal -> Formula Literal -> IO Result
interpolate a b = do
    --printf "interpolate %s %s\n" (show a) (show b)
    p <- emptyProof
    let n = max (Formula.maxVar a) (Formula.maxVar b)
        (x,xs) = tseitin (n+1) a
        a' = [x]:xs
        n' = max (maxVarClause a') n
        (y,ys) = tseitin (n' + 1) b
        b' = [y]:ys
        -- NOTE how -0 is added to simulate T
        cnf = [[Neg 0]] ++ a' ++ b'
        n'' = maxVarClause cnf
    --printf "\t cnf=%s\n" (show cnf)
    ok <- runSolverWithProof (mkProofLogger p a' b' B) $ do
        replicateM_ (fromIntegral n'' + 1) newVar
        mapM_ addClause cnf
        solve
        isOkay
    if ok
        then return Satisfiable
        else do
            i <- extractInterpolant p
            return (Unsatisfiable i)


implies :: Formula Literal -> Formula Literal -> IO Bool
implies q' q = do
    --printf "implies %s %s\n" (show q') (show q)
    not <$> sat (And [q', Not q])

sat :: Formula Literal -> IO Bool
sat f = runSolver $ do
    --liftIO $ printf "sat %s\n" (show f)
    let n = Formula.maxVar f
        (x,xs) = tseitin (n+1) f
        -- NOTE how -0 is added to simualte T
        cnf = [Neg 0]:[x]:xs
        n' = maxVarClause cnf
    --liftIO $ printf "\tcnf=%s\n" (show cnf)
    replicateM_ (fromIntegral n' + 1) newVar
    mapM_ addClause cnf
    solve
    isOkay
