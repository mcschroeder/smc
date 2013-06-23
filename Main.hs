{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Foldable hiding (mapM_,sequence_)
import Data.Word
import System.Environment

import Prelude hiding (foldr,concat)

import MiniSat
import Formula
import Aiger


-- TODO: remove
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM


-- some example formulas
f0 = Or [Lit (Pos 0), Lit (Pos 0)]
f1 = Or [Lit (Pos 0), Lit (Pos 1)]
f2 = And [Lit (Pos 0), Lit (Pos 1)]
f3 = Or [And [Lit (Pos 0), Lit (Pos 1)], And [Lit (Pos 2), Lit (Pos 3)]]
f4 = Or [And [Lit (Pos 0), Not (Lit (Pos 1))], Lit (Pos 2)]
f5 = Or [And [Lit (Pos 0), Lit (Neg 1)], Lit (Pos 2)]

-- generates formulas with alternating and / or connectives, for testing
generateFormula :: Word -> Formula Literal
generateFormula = snd . go 0
    where
        go x 0 = (x+1, Lit (Pos x))
        go x n = let (x1,f1) = go x  (n-1)
                     (x2,f2) = go x1 (n-1)
                 in if even n
                    then (x2, Or [f1,f2])
                    else (x2, And [f1,f2])


main = do
    args <- getArgs
    let file = args !! 0
        k = read $ args !! 1        
    parseAiger file >>= \case
        Left err  -> print err
        Right aag -> do
            let cnf = unwind aag k
                maxVar = var $ foldr max (Pos 0) $ concat cnf
            print cnf
            runSolver $ do
                replicateM_ (fromIntegral maxVar + 1) newVar
                mapM_ addClause cnf
                solve
                isOkay >>= \case
                    True  -> liftIO $ putStrLn "OK"
                    False -> liftIO $ putStrLn "FAIL"
                liftIO . printLines . toList =<< proof

printLines :: Show a => [a] -> IO ()
printLines = sequence_ . imap (\i a -> putStrLn (show i ++ ": " ++ show a))

imap :: (Int -> a -> b) -> [a] -> [b]
imap = go 0
    where 
        go _ _ []     = []
        go n f (x:xs) = f n x : go (n+1) f xs

solveFormula f = 
    runSolver $ do
        addFormula f
        liftIO . print =<< nVars
        liftIO . print =<< isOkay
        liftIO $ print "solving..."
        solve
        liftIO . print =<< isOkay


addFormula :: Formula Literal -> Solver ()
addFormula f = do
    replicateM_ (fromEnum $ maxVar f) newVar   -- add atom vars
    x <- tseitin f
    addUnit x
    where 
        tseitin :: Formula Literal -> Solver Literal
        tseitin (Lit x) = return x
        
        tseitin (Not f) = do
            x <- newVar
            y <- tseitin f
            addBinary (Neg x) (neg y)
            addBinary (Pos x) y
            return (Pos x)

        tseitin (Or fs) = do
            x <- newVar
            ys <- mapM tseitin fs
            mapM_ (addBinary (Pos x) . neg) ys
            addClause $ (Neg x) : ys
            return (Pos x)
        
        tseitin (And fs) = do
            x <- newVar
            ys <- mapM tseitin fs
            mapM_ (addBinary (Neg x)) ys
            addClause $ (Pos x) : map neg ys
            return (Pos x)

        maxVar :: Formula Literal -> Var
        maxVar = foldr max' 0
            where
                max' (Pos a) b = max a b
                max' (Neg a) b = max a b

