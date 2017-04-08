{-# LANGUAGE RankNTypes #-}
module HCArr where

import Data.Array.MArray
import Data.Array.IO
import Data.Random.Normal
import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Random

import Test.QuickCheck.Monadic

import TSP
import Greedy

type ArrPath = IOArray Int Vertex

mkArrPath :: Size -> [Vertex] -> IO ArrPath
mkArrPath size vs
	| size <= 0 = error "Size can't be less than 0."
	| otherwise = newListArray (1, size) vs

readArrPath :: IO ArrPath
readArrPath = do
	(size, vs) <- readInput
	mkArrPath size vs

greedyArrPath :: IO ArrPath
greedyArrPath = do
	(size, vs) <- readInput
	mkArrPath size $ greedyV vs

randomArrPath :: IO ArrPath
randomArrPath = do
	ap <- readArrPath
	randomize ap
	return ap

randomize :: ArrPath -> IO ()
randomize ap = do
	size <- getSize ap
	replicateM size $ arrSwapRnd ap
	return ()

readArrPathFromFile :: String -> IO ArrPath
readArrPathFromFile file = do
	(size, vs) <- readInputFromFile file
	mkArrPath size vs

getSize :: ArrPath -> IO Size
getSize ap = liftM snd $ getBounds ap

arrSwap :: ArrPath -> Int -> Int -> IO ()
arrSwap ap i j = do
	pi <- readArray ap i
	pj <- readArray ap j
	writeArray ap i pj
	writeArray ap j pi

arrSwapRnd :: ArrPath -> IO ()
arrSwapRnd ap = do
	(lo, hi) <- getBounds ap
	i <- randomRIO (lo + 1, hi)
	j <- randomRIO (lo + 1, hi)
	arrSwap ap i j

arrPathLen :: ArrPath -> IO Double
arrPathLen ap = do
	(lo, hi) <- getBounds ap
	vs <- mapM (\i -> readArray ap i) [lo..hi]
	return $ len vs

lenDiff :: ArrPath -> Int -> Int -> IO Double
lenDiff ap i j = lenDiff' ap (min i j) (max i j)

lenDiff' :: ArrPath -> Int -> Int -> IO Double
lenDiff' ap i j = do
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

data Config = Config
	{ numOfIter :: Int
	, swapsPerIter :: Int
	, restartThreshold :: Double
	, swapsPerRestart :: Int
	, chanel :: TVar Double
	, logging :: Bool
	}

data ClimbState = ClimbState
	{ curIter :: Int
	, maxIter :: Int
	, cur :: ArrPath
	, curLen :: Double
	, bestLen :: Double
	}

checkEnd :: StateT ClimbState IO Bool
checkEnd = do
	st <- get
	return $ curIter st > maxIter st
{-StateT $ \st -> return $ (curIter st > maxIter st, st)-}

type Init2 a = IO a
type Tweak2 = StateT ClimbState IO Double
type Restart2 = StateT ClimbState IO ()
type Log = StateT ClimbState IO ()

arrClimb :: Init2 ArrPath -> Tweak2 -> Restart2 -> Log -> Config -> IO ()
arrClimb init tweak restart log cfg = do
	initPath <- init
	len <- arrPathLen initPath

	atomically $ writeTVar (chanel cfg) len

	evalStateT climb $ ClimbState {curIter = 0, maxIter = numOfIter cfg, cur = initPath, curLen = len, bestLen = len} where

	climb :: StateT ClimbState IO ()
	climb = do
		end <- checkEnd
		if end
		then return ()
		else do
			st <- get
			restart
			diff <- tweak
			--lift $ hPutStrLn stderr $ show diff
			log
			lift $ when (curLen st + diff < bestLen st) $ do
				atomically $ writeTVar (chanel cfg) $ curLen st + diff
			modify $ \st -> st {curIter = curIter st + 1, curLen = curLen st + diff, bestLen = min (bestLen st) (curLen st + diff)}
			newLen <- lift $ arrPathLen $ cur st
			lift $ when (abs (newLen - curLen st - diff) > 1e-5) $ do
				error $ "newLen = " ++ (show newLen) ++ ", curLen = " ++ (show $ curLen st) ++ ", diff = " ++ (show diff) ++ ", curLen + diff = " ++ (show $ curLen st + diff)
			
			{-lift $ when (newLen < bestLen st) $ do
				atomically $ writeTVar (chanel cfg) $ newLen
			modify $ \st -> st {curIter = curIter st + 1, curLen = newLen, bestLen = min newLen (bestLen st)}-}
			climb

newArrTweak :: Int -> Tweak2
newArrTweak swapsPerIter = do
	st <- get
	let ap = cur st
	(lo, hi) <- lift $ getBounds ap
	swaps <- lift $ replicateM swapsPerIter $ do
		i <- randomRIO (lo + 1, hi)
		j <- randomRIO (lo + 1, hi)
		return (i, j)
	(i, j) <- lift $ foldM (\(i, j) (i', j') -> do
		len <- lenDiff ap i j
		len' <- lenDiff ap i' j'
		return $ if len < len' then (i, j) else (i', j')) (head swaps) (tail swaps)
	diff <- lift $ lenDiff ap i j
	lift $ arrSwap ap i j
	lift $ return diff

progressiveTweak :: Int -> Tweak2
progressiveTweak swapsPerIter = do
	st <- get
	newArrTweak $ floor $ fromIntegral swapsPerIter * (1.5 ^ (curIter st `div` 25000))

tabuTweak :: Int -> Double -> Tweak2
tabuTweak numOfSamples threshold = do
	st <- get
	let ap = cur st
	(lo, hi) <- lift $ getBounds ap
	swaps <- lift $ replicateM numOfSamples $ do
		i <- randomRIO (lo + 1, hi)
		j <- randomRIO (lo + 1, hi)
		return (i, j)
	swaps' <- lift $ sequence $ map (\(i, j) -> do
		l <- lenDiff ap i j
		return (i, j, l)) swaps
	let	swaps'' = swaps' $> sortBy (\(_, _, l) (_, _, l') -> compare l l') $> takeWhile (\(_, _, l) -> l < threshold) $> tabu
		diff = swaps'' $> map (\(_, _, l) -> l) $> sum
	lift $ sequence $ map (\(i, j, _) -> arrSwap ap i j) swaps''
	return diff

	where	tabu :: [(Int, Int, Double)] -> [(Int, Int, Double)]
		tabu [] = []
		tabu (x@(i, j, _):xs) = x : (tabu $ filter (\(i', j', _) -> i == i' || i == j' || j == i' || j == j') xs)

hybridTweak :: Int -> Double -> Tweak2
hybridTweak numOfSamples threshold = do
	st <- get
	if curIter st < 50000
	then tabuTweak numOfSamples threshold
	else newArrTweak numOfSamples

newNoRestart :: Restart2
newNoRestart = lift $ return ()

arrRestart :: Double -> Int -> Restart2
arrRestart threshold numOfSwaps = do
	st <- get
	r <- lift $ normalIO' (0.0, 1.0)
	lift $ when (r > threshold) $ do
		replicateM_ numOfSwaps (arrSwapRnd $ cur st)
	return ()

logStateToStdErr :: Log
logStateToStdErr = do
	st <- get
	lift $ when (curIter st `mod` 100 == 0) $ do
		hPutStrLn stderr $ "iter = " ++ (show $ curIter st) ++ "/" ++ (show $ maxIter st) ++ ", curLen = " ++ (show $ curLen st) ++ ", bestLen = " ++ (show $ bestLen st)

arrayTest :: IO ()
arrayTest = do
	let	init = readArrPath

	chanel <- atomically $ newTVar 0.0
	forkIO $ TSP.logBest chanel

	arrClimb init (hybridTweak 50 100.0) (arrRestart 3.5 10) logStateToStdErr $ Config
		{ numOfIter = 10^6
		, swapsPerIter = 42
		, restartThreshold = 2.0
		, swapsPerRestart = 5
		, chanel = chanel
		, logging = True
		}

smallData :: IO ()
smallData = do

	chanel <- atomically $ newTVar 0.0
	forkIO $ TSP.logBest chanel

	arrClimb greedyArrPath (newArrTweak 25) newNoRestart logStateToStdErr $ Config
		{ numOfIter = 10^6
		, swapsPerIter = 42
		, restartThreshold = 2.0
		, swapsPerRestart = 5
		, chanel = chanel
		, logging = True
		}
	

--main = smallData

{-
	Ulepszyć algorytm zachłanny.
	Restart jest zbugowany.

-}
