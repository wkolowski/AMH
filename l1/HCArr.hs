{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Array.MArray
import Data.Array.IO
import Data.Random.Normal

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Random

import TSP
import Greedy

data ArrPath = ArrPath (IOArray Int Vertex) deriving Eq

mkArrPath :: Size -> [Vertex] -> IO ArrPath
mkArrPath size vs
	| size <= 0 = error "Size can't be less than 0."
	| otherwise = liftM ArrPath $ newListArray (1, size) vs

getSize :: ArrPath -> IO Size
getSize ap@(ArrPath p) = liftM snd $ getBounds p

readArrPath :: IO ArrPath
readArrPath = do
	(size, vs) <- readInput
	mkArrPath size vs

greedyArrPath :: IO ArrPath
greedyArrPath = do
	(size, vs) <- readInput
	mkArrPath size $ greedyV vs

readArrPathFromFile :: String -> IO ArrPath
readArrPathFromFile file = do
	(size, vs) <- readInputFromFile file
	mkArrPath size vs

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
	return $ len vs

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


data Config = Config
	{ numOfIter :: Int
	, swapsPerIter :: Int
	, restartThreshold :: Float
	, swapsPerRestart :: Int
	, chanel :: TVar Float
	, logging :: Bool
	}

data ClimbState = ClimbState
	{ curIter :: Int
	, maxIter :: Int
	, cur :: ArrPath
	, curLen :: Float
	, bestLen :: Float
	}

checkEnd :: StateT ClimbState IO Bool
checkEnd = do
	st <- get
	return $ curIter st > maxIter st
{-StateT $ \st -> return $ (curIter st > maxIter st, st)-}

type Init2 a = IO a
type Tweak2 = StateT ClimbState IO Float
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
			log
			lift $ when (curLen st + diff < bestLen st) $ do
				atomically $ writeTVar (chanel cfg) $ curLen st + diff
			modify $ \st -> st {curIter = curIter st + 1, curLen = curLen st + diff, bestLen = min (bestLen st) (curLen st + diff)}
			climb

newArrTweak :: Int -> Tweak2
newArrTweak swapsPerIter = do
	st <- get
	let ap@(ArrPath p) = cur st
	(lo, hi) <- lift $ getBounds p
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

newNoRestart :: Restart2
newNoRestart = lift $ return ()

logStateToStdErr :: Log
logStateToStdErr = do
	st <- get
	lift $ when (curIter st `mod` 10 == 0) $ do
		hPutStrLn stderr $ "iter = " ++ (show $ curIter st) ++ "/" ++ (show $ maxIter st) ++ ", curLen = " ++ (show $ curLen st) ++ ", bestLen = " ++ (show $ bestLen st)

arrayTest :: IO ()
arrayTest = do
	let	init = readArrPath

	chanel <- atomically $ newTVar 0.0
	forkIO $ TSP.logBest chanel

	Main.arrClimb init (progressiveTweak 25) newNoRestart logStateToStdErr $ Config
		{ numOfIter = 10^6
		, swapsPerIter = 42
		, restartThreshold = 2.0
		, swapsPerRestart = 5
		, chanel = chanel
		, logging = True
		}

main = Main.arrayTest
