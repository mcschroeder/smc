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
--import Proof
import Interpolation2

--import qualified DynamicVector as V

main :: IO ()
main = do
    args <- getArgs
    let file = args !! 0
    parseAiger file >>= \case
        Left err -> print err
        Right aag -> checkAiger aag >>= \case
            True -> putStrLn "\nOK"
            False -> putStrLn "\nFAIL"

simple = either undefined return =<< parseAiger "simple.aag"
simple_err = either undefined return =<< parseAiger "simple_err.aag"

cnf2f :: [Clause] -> Formula Literal
cnf2f = simplify . And . map (Or . map Lit)

--f2cnf :: Formula Literal -> [Clause]
--f2cnf f = let n = Formula.maxVar f
--              (x,xs) = tseitin (n+1) f
--          in ([x]:xs)

maxVarClause :: [Clause] -> Var
maxVarClause = var . foldr max (Pos 0) . concat


----------------------------

trans :: Aiger -> Int -> [Clause] -> [Clause] -> ([Clause],[Clause])
trans aag k ts ([Pos n]:fk) = (ts',fk')
    where
        (q,t,f') = unwind' aag k
        fk' = f' ++ ([Neg n] : fk)
        ts' = q ++ t ++ ts

checkAiger :: Aiger -> IO Bool
checkAiger aag = do
    let (q0, t0_, f0) = unwind' aag 0
        i0 = cnf2f q0
        t0 = cnf2f t0_
    check 0 i0 t0 [] f0 aag

andsToClause :: Formula Literal -> [Clause]
andsToClause (And xs) = go xs []
    where
        go [] cs = cs
        go ((Lit t):xs) cs = go xs ([t]:cs)
        go ((Or ts):xs) cs = go xs ([(go2 ts [])] ++ cs)
            where
                go2 [] c = c
                go2 ((Lit x):ts) c = go2 ts (x:c)

check :: Int -> Formula Literal -> Formula Literal -> [Clause] -> [Clause] -> Aiger -> IO Bool
check k i0 t0 ts fk aag = do
    --printf "\ncheck %s %s %s %s %s\n" (show k) (show i0) (show t0) (show ts) (show fk)
    printf "\ncheck\n"
    --let f = if null ts
    --            then And [i0, t0, cnf2f [fk]]
    --            else And [i0, t0, cnf2f ts, cnf2f [fk]]
    --sat f >>= \case

    let f = andsToClause i0 ++ andsToClause t0 ++ ts ++ fk
    satClauses f >>= \case
        True -> return True
        False -> do
            let r = i0
                a = And [i0, t0]
                b = And [cnf2f ts, cnf2f fk]

            fixpoint a b r t0 aag k >>= \case
                True -> return False
                False -> do
                    let (ts',fk') = trans aag (k+1) ts fk
                    check (k+1) i0 t0 ts' fk' aag


-- TODO: rewind i to 0 time frame?
fixpoint a b r t0 aag k= do
    --printf "fixpoint %s %s %s %s\n" (show a) (show b) (show r) (show t0)
    printf "fixpoint\n"
    interpolate a b >>= \case
        Satisfiable -> return True
        Unsatisfiable a' -> do
            a' `implies` r >>= \case
                True -> return False
                False -> do
                    let r' = Or [a', r]
                        a'' = And [a', t0]
                    fixpoint a'' b r' t0 aag k


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
        cnf = {-[Neg 0]:-}[x]:xs
        n' = maxVarClause cnf
    --liftIO $ printf "\tcnf=%s\n" (show cnf)
    replicateM_ (fromIntegral n' + 1) newVar
    mapM_ addClause cnf
    solve
    isOkay

satClauses :: [Clause] -> IO Bool
satClauses cs = runSolver $ do
    --liftIO $ printf "satClauses %s\n" (show cs)
    let cnf = [[Neg 0]] ++ cs
        n = maxVarClause cnf
    --liftIO $ printf "\t cnf=%s\n" (show cnf)
    replicateM_ (fromIntegral n + 1) newVar
    mapM_ addClause cnf
    solve
    isOkay

----------------------------

--checkAiger :: Aiger -> IO Bool
--checkAiger aag = do
--    let (t0, p0) = unwind' aag 0
--        q0' = [[Neg 0]]  --T

--        step :: [Clause] -> Int -> [Clause]
--        step (((Pos n):ps):b) k = (p : (Neg n) : ps) : (t ++ b)
--        --step ([Pos n]:b) k = p ++ t ++ [Neg n] : b
--            where (t,p) = unwind' aag k

--    check q0' t0 [[p0]] 0 step

--    --let (t1, p1) = unwind' aag 1
--    --check q0' (t0 ++ t1) [[neg p0],[p1]] 1 step



--check :: [Clause] -> [Clause] -> [Clause] -> Int -> ([Clause] -> Int -> [Clause]) -> IO Bool
--check q0 t0 b k step = do
--    printf "\ncheck %s %s %s %d\n" (show q0) (show t0) (show b) k
--    --printf "check\n"
--    let a = q0 ++ t0
--    solveAndInterpolate a b >>= \case
--        Satisfiable -> return True
--        Unsatisfiable i -> do
--            let n = var $ foldr max (Pos 0) $ concat b
--            fix i (cnf2f q0) t0 b >>= \case
--                True -> return False
--                False -> do
--                    let b' = step b (k+1)
--                    check q0 t0 b' (k+1) step

---- TODO: rewind i to 0 time frame
--fix :: Formula Literal -> Formula Literal -> [Clause] -> [Clause] -> IO Bool
--fix i q t0 b = do
--    printf "fix %s %s %s %s\n" (show i) (show q) (show t0) (show b)
--    --printf "fix\n"
--    let q' = Or [i, q]
--    q' `implies` q >>= \case
--        True -> return True
--        False -> do
--            let a = (f2cnf q') ++ t0
--            solveAndInterpolate a b >>= \case
--                Satisfiable -> return False
--                Unsatisfiable i' -> fix i' q' t0 b

--implies :: Formula Literal -> Formula Literal -> IO Bool
--implies q' q = runSolver $ do
--    --liftIO $ printf "implies %s %s\n" (show q') (show q)
--    --liftIO $ printf "implies\n"
--    let n = max (Formula.maxVar q) (Formula.maxVar q')
--        (x,xs) = tseitin (n+1) $ And [q', Not q]
--        f = ([x]:xs)
--        n' = maxVarClause f
--    replicateM_ (fromIntegral n' + 1) newVar
--    mapM_ addClause f
--    solve
--    not <$> isOkay



---- TODO: choice of system
--solveAndInterpolate :: [Clause] -> [Clause] -> IO Result
--solveAndInterpolate a b = do
--    printf "solveAndInterpolate %s %s\n" (show a) (show b)
--    --printf "solveAndInterpolate\n"
--    p <- emptyProof
--    ok <- runSolverWithProof (mkProofLogger p a b B) $ do
--        let n = maxVarClause (a ++ b)
--        replicateM_ (fromIntegral n + 1) newVar
--        mapM_ addClause a
--        mapM_ addClause b
--        solve
--        isOkay
--    if ok
--        then return Satisfiable
--        else do
--            i <- extractInterpolant p
--            return (Unsatisfiable i)
