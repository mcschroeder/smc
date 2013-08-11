module DynamicVector
	( DynamicVector
	, new, length, append, unsafeRead, unsafeWrite, freeze, imap_
    , printVector
	) where

import Control.Applicative
import Data.IORef
import Data.Vector (Vector)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Prelude hiding (length)

-- TODO: doubling strategy as parameter
-- (resoltuion proofs tend to be around the length of the cnf,
-- so initializing with that & then growing w/ 100 or so could be good)

-- | A mutable vector in the IO monad that grows automatically to accomodate
-- its elements. The growth strategy is to simply double the internal size
-- of the vector once it is full.
data DynamicVector a = DynamicVector (IORef (IOVector a)) (IORef Int)

-- | Create a dynamic vector with the given initial capacity.
new :: Int -> IO (DynamicVector a)
new m = do
    vref <- newIORef =<< VM.new m
    nref <- newIORef 0
    return $ DynamicVector vref nref

-- | Length of the dynamic vector. (This is the number of actual elements
-- in the vector. The amount of storage consumed may be higher.)
length :: DynamicVector a -> IO Int
length (DynamicVector _ nref) = readIORef nref

-- | Append an item to the vector. May double the storage claimed by the vector.
append :: DynamicVector a -> a -> IO ()
append (DynamicVector vref nref) x = do
    v <- readIORef vref
    n <- readIORef nref
    if n < VM.length v
        then VM.unsafeWrite v n x
        else do v' <- VM.unsafeGrow v n
                writeIORef vref v'
                VM.unsafeWrite v' n x
    writeIORef nref (n+1)

-- | Yield the element at the given position. No bounds checks are performed.
unsafeRead :: DynamicVector a -> Int -> IO a
unsafeRead (DynamicVector vref _) i = do
    v <- readIORef vref
    VM.unsafeRead v i

-- | Replace the element at the given position. No bounds checks are performed.
unsafeWrite :: DynamicVector a -> Int -> a -> IO ()
unsafeWrite (DynamicVector vref _) i x = do
    v <- readIORef vref
    VM.unsafeWrite v i x

-- | /O(n)/ Yield an immutable copy.
freeze :: DynamicVector a -> IO (Vector a)
freeze dv = V.freeze =<< activeSlice dv

-- | /O(n)/ Apply the IO action to every element of the vector and its index
-- and ignore the results.
imap_ :: (Int -> a -> IO b) -> DynamicVector a -> IO ()
imap_ f dv = go 0 f =<< V.toList <$> (V.unsafeFreeze =<< activeSlice dv)
    where
        go _ _ []     = return ()
        go n f (x:xs) = f n x >> go (n+1) f xs

activeSlice :: DynamicVector a -> IO (IOVector a)
activeSlice (DynamicVector vref nref) =
    VM.unsafeSlice 0 <$> readIORef nref <*> readIORef vref

printVector :: Show a => DynamicVector a -> IO ()
printVector v = flip imap_ v $ \i x -> do
    putStrLn (show i ++ ": " ++ show x)
