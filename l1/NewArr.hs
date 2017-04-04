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

import qualified TSP as TSP
import HCArr

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
type Tweak2 = StateT ClimbState IO ()
type Restart2 = StateT ClimbState IO ()

arrClimb :: Init2 ArrPath -> Tweak2 -> Restart2 -> Config -> IO ()
arrClimb init tweak restart cfg = do
	initPath <- init
	len <- arrPathLen initPath

	evalStateT climb $ ClimbState {curIter = 0, maxIter = numOfIter cfg, cur = initPath, curLen = len, bestLen = len} where

	climb :: StateT ClimbState IO ()
	climb = do
		end <- checkEnd
		if end
		then return ()
		else do
			st <- get
			restart
			tweak
			newLen <- lift $ (arrPathLen $ cur st)
			lift $ when (logging cfg && curIter st `mod` 10 == 0) $ do
				hPutStrLn stderr $ "iter = " ++ (show $ curIter st) ++ "/" ++ (show $ maxIter st) ++ ", curLen = " ++ (show $ curLen st) ++ ", bestLen = " ++ (show $ bestLen st)
			lift $ when (newLen <= bestLen st) $ do
				atomically $ writeTVar (chanel cfg) newLen
			modify $ \st -> st {curIter = curIter st + 1, curLen = newLen, bestLen = min newLen (bestLen st)}
			climb

newArrTweak :: Int -> Tweak2
newArrTweak swapsPerIter = do
	st <- get
	let ap@(ArrPath p) = cur st
	(lo, hi) <- lift $ getBounds p
	oldLen <- lift $ arrPathLen ap
	swaps <- lift $ replicateM swapsPerIter $ do
		i <- randomRIO (lo + 1, hi)
		j <- randomRIO (lo + 1, hi)
		return (i, j)
	(i, j) <- lift $ foldM (\(i, j) (i', j') -> do
		len <- lenDiff ap i j
		len' <- lenDiff ap i' j'
		return $ if len < len' then (i, j) else (i', j')) (head swaps) (tail swaps)
	lift $ arrSwap ap i j

newNoRestart :: Restart2
newNoRestart = lift $ return ()

arrayTest :: Int -> IO ()
arrayTest swapsPerIter = do
	let	init = readArrPath

	chanel <- atomically $ newTVar 0.0
	forkIO $ TSP.logBest chanel

	Main.arrClimb init (newArrTweak swapsPerIter) newNoRestart $ Config
		{ numOfIter = 10^5
		, swapsPerIter = swapsPerIter
		, restartThreshold = 2.0
		, swapsPerRestart = 5
		, chanel = chanel
		, logging = True
		}

main = Main.arrayTest 75
