{-# LANGUAGE RankNTypes #-}
module HCArr where

import Data.Array.MArray
import Data.Array.IO
import Data.Random.Normal

-- import Control.Applicative

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Random

import TSP
import Greedy

data ArrPath = ArrPath (IOArray Int Vertex) deriving Eq

mkArrPath :: Size -> Path -> IO ArrPath
mkArrPath size (Path vs)
	| size <= 0 = error "Size can't be less than 0."
	| otherwise = liftM ArrPath $ newListArray (1, size) vs

getSize :: ArrPath -> IO Size
getSize ap@(ArrPath p) = liftM snd $ getBounds p

readArrPath :: IO ArrPath
readArrPath = do
	(size, p) <- readInput
	mkArrPath size p

greedyArrPath :: IO ArrPath
greedyArrPath = do
	(size, p) <- readInput
	mkArrPath size $ greedy p

readArrPathFromFile :: String -> IO ArrPath
readArrPathFromFile file = do
	(size, p) <- readInputFromFile file
	mkArrPath size p

arrSwap :: ArrPath -> Int -> Int -> IO ()
arrSwap (ArrPath p) i j = do
	pi <- readArray p i
	pj <- readArray p j
	writeArray p i pj
	writeArray p j pi

arrSwapRnd :: ArrPath -> IO ()
arrSwapRnd ap@(ArrPath p) = do
	(lo, hi) <- getBounds p
	i <- randomRIO (lo + 1, hi)
	j <- randomRIO (lo + 1, hi)
	arrSwap ap i j

arrPathLen :: ArrPath -> IO Float
arrPathLen (ArrPath p) = do
	(lo, hi) <- getBounds p
	vs <- mapM (\i -> readArray p i) [lo..hi]
	return $ pathLen (Path vs)

lenDiff :: ArrPath -> Int -> Int -> IO Float
lenDiff ap i j = lenDiff' ap (min i j) (max i j)

lenDiff' :: ArrPath -> Int -> Int -> IO Float
lenDiff' (ArrPath ap) i j = do
	(_, size) <- getBounds ap

	pi <- readArray ap i
	pj <- readArray ap j

	pi_prev <- readArray ap (i - 1)
	pi_next <- readArray ap (i `mod` size + 1)
	pj_prev <- readArray ap (j - 1)
	pj_next <- readArray ap (j `mod` size + 1)

	let	i_old_prev = dist pi pi_prev
		i_old_next = dist pi pi_next
		j_old_prev = dist pj pj_prev
		j_old_next = dist pj pj_next
	
		i_new_prev = dist pi pj_prev
		i_new_next = dist pi pj_next
		j_new_prev = dist pj pi_prev
		j_new_next = dist pj pi_next

	if abs (i - j) == 1
	then
		return $ i_new_next + j_new_prev - i_old_prev - j_old_next
	else
		return $ i_new_prev + i_new_next + j_new_prev + j_new_next - (i_old_prev + i_old_next + j_old_prev + j_old_next)

arrTweak :: Tweak ArrPath
arrTweak dom = do
	let ap@(ArrPath p) = curPath dom
	(lo, hi) <- getBounds p
	oldLen <- arrPathLen ap
	i <- randomRIO (lo + 1, hi)
	j <- randomRIO (lo + 1, hi)
	pi <- readArray p i
	pj <- readArray p j
	writeArray p i pj
	writeArray p j pi
	newLen <- arrPathLen ap
	when (newLen > oldLen) $ do
		writeArray p i pi
		writeArray p j pj
	return ap

arrTweak2 :: Int -> Tweak ArrPath
arrTweak2 numOfSwaps dom = do
	let ap@(ArrPath p) = curPath dom
	(lo, hi) <- getBounds p
	oldLen <- arrPathLen ap
	swaps <- replicateM numOfSwaps $ do --(25 + curIter dom `div` 1000) $ do
		i <- randomRIO (lo + 1, hi)
		j <- randomRIO (lo + 1, hi)
		return (i, j)
	(i, j) <- foldM (\(i, j) (i', j') -> do
		len <- lenDiff ap i j
		len' <- lenDiff ap i' j'
		return $ if len < len' then (i, j) else (i', j')) (head swaps) (tail swaps)
	
	pi <- readArray p i
	pj <- readArray p j
	writeArray p i pj
	writeArray p j pi
	return ap

arrTweak3 :: Int -> Tweak ArrPath
arrTweak3 numOfSwaps dom = do
	let ap@(ArrPath p) = curPath dom
	(lo, hi) <- getBounds p
	oldLen <- arrPathLen ap
	swaps <- replicateM numOfSwaps $ do
		i <- randomRIO (lo + 1, hi)
		j <- randomRIO (lo + 1, hi)
		return (i, j)
	(i, j) <- foldM (\(i, j) (i', j') -> do
		len <- lenDiff ap i j
		len' <- lenDiff ap i' j'
		return $ if len < len' then (i, j) else (i', j')) (head swaps) (tail swaps)
	diff <- lenDiff ap i j
	when (diff < 50.0) $ do
		pi <- readArray p i
		pj <- readArray p j
		writeArray p i pj
		writeArray p j pi
	return ap

arrTweak4 :: Int -> Tweak ArrPath
arrTweak4 numOfSwaps dom = arrTweak2 (numOfSwaps * 2 ^ ((curIter dom `div` 10000) `mod` 3)) dom {-if curIter dom `mod` 10000 == 0
	then arrTweak4 (2 * numOfSwaps) $ dom {curIter = curIter dom + 1}
	else arrTweak2 numOfSwaps dom-}

arrRestart :: Float -> Int -> Restart ArrPath
arrRestart threshold numOfSwaps dom = do
	r <- normalIO' (0.0, 1.0) :: IO Float
	when (r > threshold) $ do
		replicateM_ numOfSwaps (arrSwapRnd $ curPath dom)
	return $ curPath dom

arrClimb :: TVar Float -> Tweak ArrPath -> Restart ArrPath -> Domain ArrPath Float -> IO ()
arrClimb best tweak restart dom = do
	let bestLen = bestPath dom

	climb' dom where

	climb' dom = do
		if curIter dom > maxIter dom
		then return () -- $ bestPath dom
		else do
			restart dom
			let bestLen = bestPath dom
			tweak dom
			newLen <- arrPathLen $ curPath dom
			when (logging dom && curIter dom `mod` 10 == 0) $ do
				hPutStrLn stderr $ "size = " ++ (show $ size dom) ++ ", iter = " ++ (show $ curIter dom) ++ "/" ++ (show $ maxIter dom) ++ ", curPath = " ++ (show $ newLen) ++ ", bestPath = " ++ (show $ bestLen)
			if newLen <= bestLen
			then do
				atomically $ writeTVar best newLen
				climb' $ dom {curIter = curIter dom + 1, bestPath = newLen}
			else do
				climb' $ dom {curIter = curIter dom + 1}

arrayTest :: Int -> Int -> ArrPath -> IO ()
arrayTest numOfSwaps tweakSize ap = do
	replicateM_ numOfSwaps (arrSwapRnd ap)
	size <- getSize ap
	bestLen <- arrPathLen ap
	channel <- atomically $ newTVar bestLen
	forkIO $ logBest channel
	arrClimb channel (arrTweak4 tweakSize) noRestart $ Domain {size = size, curIter = 0, maxIter = 10000000, curPath = ap, bestPath = bestLen, logging = True}
	return ()


--main = arrayTest 0 50 <$> readArrPath

{-main = do
	--ap <- greedyArrPath
	ap <- readArrPath
	arrayTest 0 50 ap-}
