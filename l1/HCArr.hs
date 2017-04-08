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

import TSP

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
	mkArrPath size $ greedy vs

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
	, curLens :: [Double]
	, bestLen :: Double
	, bestLens :: [Double]
	, tabu :: [Int]
	}

type Init a = IO a
type End = StateT ClimbState IO Bool
type Tweak = StateT ClimbState IO Double
type Restart = StateT ClimbState IO ()
type Log = StateT ClimbState IO ()

climb :: Init ArrPath -> End -> Tweak -> Restart -> Log -> Config -> IO ()
climb init checkEnd tweak restart log cfg = do
	initPath <- init
	len <- arrPathLen initPath

	atomically $ writeTVar (chanel cfg) len

	evalStateT climb' $ ClimbState
		{ curIter = 0
		, maxIter = numOfIter cfg
		, cur = initPath
		, curLen = len
		, curLens = [len]
		, bestLen = len
		, bestLens = [len]
		, tabu = []
		} where

	climb' :: StateT ClimbState IO ()
	climb' = do
		end <- checkEnd
		if end
		then return ()
		else do
			st <- get
			--restart
			diff <- tweak
			let	new = curLen st + diff
				best = min (bestLen st) new
			log
			lift $ when (new < bestLen st) $ do --(curLen st + diff < bestLen st) $ do
				atomically $ writeTVar (chanel cfg) $ new --curLen st + diff
			--modify $ \st -> st {curIter = curIter st + 1, curLen = curLen st + diff, bestLen = min (bestLen st) (curLen st + diff)}
			modify $ \st -> st {curIter = curIter st + 1, curLen = new, curLens = new : curLens st, bestLen = best, bestLens = best : bestLens st}
			climb'

enoughIterEnd :: End
enoughIterEnd = do
	st <- get
	return $ curIter st > maxIter st

noImprovementEnd :: Int -> End
noImprovementEnd threshold = do
	st <- get
	if curIter st < threshold
	then return False
	else return $ head (bestLens st) == bestLens st !! threshold

divergingEnd :: Int -> Int -> Double -> End
divergingEnd initialIters n threshold = do
	st <- get
	if curIter st < initialIters
	then return False
	else return $ head (curLens st) - curLens st !! n > threshold

generateSamples :: ArrPath -> Int -> StateT ClimbState IO [(Int, Int)]
generateSamples ap n = do
	(lo, hi) <- lift $ getBounds ap
	lift $ replicateM n $ do
		i <- randomRIO (lo + 1, hi)
		j <- randomRIO (lo + 1, hi)
		return (i, j)

simpleTweak :: Int -> Tweak
simpleTweak swapsPerIter = do
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

simpleTweak' :: Int -> Tweak
simpleTweak' numOfSamples = do
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
	let swaps'' = swaps' $> filter (\(i, j, _) -> i /= j)
	if swaps'' == []
	then lift $ return 0.0
	else do
		let (i, j, diff) = swaps'' $> minimumBy (\(_, _, l) (_, _, l') -> compare l l')
		lift $ arrSwap ap i j
		lift $ return diff

fastTweak :: Int -> Tweak
fastTweak numOfSamples = do
	ap <- liftM cur get
	samples <- generateSamples ap numOfSamples
	diff <- lift $ foldM (\p (i, j) -> do
		len <- lenDiff ap i j
		if len < 0.0
		then do
			arrSwap ap i j
			return $ len + p
		else return p) 0.0 samples
	return diff
	if diff < 0.0
	then return diff
	else simpleTweak' numOfSamples

progressiveTweak :: Int -> Tweak
progressiveTweak swapsPerIter = do
	st <- get
	simpleTweak $ floor $ fromIntegral swapsPerIter * (1.5 ^ (curIter st `div` 25000))

newTabuTweak :: Int -> Int -> Tweak
newTabuTweak numOfSamples tabuSize = do
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
	let swaps'' = swaps' $> filter (\(i, j, l) -> i /= j && (l < 25.0 || (i `notElem` take tabuSize (tabu st) && j `notElem` take tabuSize (tabu st))))
	let (i, j, diff) = swaps'' $> minimumBy (\(_, _, l) (_, _, l') -> compare l l')
	lift $ arrSwap ap i j
	modify $ \st -> st {tabu = i : j : take (tabuSize - 2) (tabu st)}
	lift $ return diff

noRestart :: Restart
noRestart = lift $ return ()

randomRestart :: Double -> Int -> Restart
randomRestart threshold numOfSwaps = do
	st <- get
	r <- lift $ normalIO' (0.0, 1.0)
	lift $ when (r > threshold) $ do
		replicateM_ numOfSwaps (arrSwapRnd $ cur st)
	return ()

noLogging :: Log
noLogging = return ()

logStateToStdErr :: Log
logStateToStdErr = do
	st <- get
	lift $ when (curIter st `mod` 100 == 0) $ do
		hPutStrLn stderr $ "iter = " ++ (show $ curIter st) ++ "/" ++ (show $ maxIter st) ++ ", curLen = " ++ (show $ curLen st) ++ ", bestLen = " ++ (show $ bestLen st) -- ++ ", tabu = " ++ (show $ take 20 (tabu st))

arrayTest :: IO ()
arrayTest = do
	chanel <- atomically $ newTVar 0.0
	forkIO $ TSP.logBest chanel

	climb randomArrPath (divergingEnd 500000 1000 100.0) (fastTweak 500) noRestart logStateToStdErr $ Config
		{ numOfIter = 10^5
		, swapsPerIter = 42
		, restartThreshold = 2.0
		, swapsPerRestart = 5
		, chanel = chanel
		, logging = True
		}
	

--main = smallData

{-
	Restart jest zbugowany.
	hybridTweak jest zbugowany

-}
