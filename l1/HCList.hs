{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module HCList where

import Data.Random.Normal

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Random

import TSP

newtype Path = Path [Vertex] --deriving Eq

instance Show Path where
	show (Path vs) = show vs

pathLen :: Path -> Double
pathLen (Path vs) = len vs

instance Eq Path where
	p == p' = pathLen p == pathLen p'

-- Paths are ordered by their length.
instance Ord Path where
	compare vs1 vs2 = compare (pathLen vs1) (pathLen vs2)

-- Hill Climbing.

data Domain a b = Domain
	{ size :: Size
	, curIter :: Int
	, maxIter :: Int
	, curPath :: a
	, bestPath :: b
	, logging :: Bool
	}

instance Show (Domain Path Path) where
	show dom = "size = " ++ (show $ size dom) ++ ", iter = " ++ (show $ curIter dom) ++ "/" ++ (show $ maxIter dom) ++ ", curPath = " ++ (show $ pathLen $ curPath dom) ++ ", bestPath = " ++ (show $ pathLen $ bestPath dom)

instance Show (Domain Path [Path]) where
	show dom = "size = " ++ (show $ size dom) ++ ", iter = " ++ (show $ curIter dom) ++ "/" ++ (show $ maxIter dom) ++ ", curPath = " ++ (show $ pathLen $ curPath dom) ++ ", bestPath = " ++ (show $ pathLen $ head $ bestPath dom)

type Init a b = IO (a, b)
type Tweak a = forall b. Domain a b -> IO a
type Select a = forall b. Domain a b -> a -> a
type Restart a = forall b. Domain a b -> IO a

replace :: Select a
replace _ new = new

keep :: Select a
keep dom _ = curPath dom

noRestart :: Restart a
noRestart dom = return $ curPath dom

-- Swap elements at positions i and j in the given list.
swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs =
	if i == j
	then xs
	else left ++ [elemJ] ++ middle ++ [elemI] ++ right
	where
		elemI = xs !! i
		elemJ = xs !! j
		left = take i xs
		middle = take (j - i - 1) (drop (i + 1) xs)
		right = drop (j + 1) xs

swap :: Size -> Path -> IO Path
swap size (Path vs) = do
	i <- randomRIO (1, size - 1)
	j <- randomRIO (1, size - 1)
	--putStrLn $ "i = " ++ (show i) ++ ", j = " ++ (show j)
	return $ Path $ swapElementsAt (min i j) (max i j) vs

swapN :: Int -> Size -> Path -> IO Path
swapN n size p = iterM n (swap size) p

randomTweak :: Int -> Tweak Path
randomTweak numOfSwaps dom = iterM numOfSwaps (swap $ size dom) (curPath dom)

bestNeighbour :: Int -> Size -> Tweak Path
bestNeighbour times size dom = liftM minimum $ replicateM times $ swap size (curPath dom)

progressiveBestNeighbour :: Tweak Path
progressiveBestNeighbour dom = liftM minimum $ replicateM (50 + (curIter dom `div` 10)) $ swap (size dom) (curPath dom)

normalNeighbour :: Int -> Size -> Tweak Path
normalNeighbour times size dom = do
	let stddev = 1 / (fromIntegral size)
	r <- normalIO' (0.0, 1 + stddev) :: IO Double
	let n = floor $ 1 + 20 * abs r
	liftM minimum $ replicateM times $ swap size (curPath dom)

climb :: TVar Double -> Init Path Path -> Tweak Path -> Select Path -> Domain Path Path -> IO Path
climb best init tweak select dom = do
	(curPath, bestPath) <- init
	atomically $ writeTVar best $ pathLen bestPath

	climb' 0 tweak select $ dom {curPath = curPath, bestPath = bestPath} where

	climb' n tweak select dom = do
		if n > maxIter dom
		then return $ bestPath dom
		else do
			new <- tweak dom
			when (logging dom) $ do
				hPutStr stderr $ show $ 50 + (curIter dom `div` 10)
				hPutStrLn stderr $ show dom
			when (new <= bestPath dom) $ do
				atomically $ writeTVar best $ pathLen new

			climb' (n + 1) tweak select $ dom {curIter = curIter dom + 1, bestPath = min new $ bestPath dom, curPath = select dom new}


climbR :: TVar Double -> Int -> Init Path [Path] -> Tweak Path -> Select Path -> Restart Path -> Domain Path [Path] -> IO Path
climbR best n init tweak select restart dom = do
	(initPath, _) <- init
	atomically $ writeTVar best $ pathLen $ initPath

	climbR' tweak select $ dom {curPath = initPath} where

	climbR' :: Tweak Path -> Select Path -> Domain Path [Path] -> IO Path
	climbR' tweak select dom = do
		if curIter dom > maxIter dom
		then return $ head $ bestPath dom
		else
			if (take n $ bestPath dom) == (replicate n (head $ bestPath dom))
			then do
				restartPath <- restart dom
				climbR' tweak select $ dom {curPath = restartPath, bestPath = [head $ bestPath dom]}
			else do
				new <- tweak dom
				when (logging dom) $ do
					hPutStrLn stderr $ show dom
				when (new <= head (bestPath dom)) $ do
					atomically $ writeTVar best $ pathLen $ head $ bestPath dom

				climbR' tweak select $ dom {curIter = curIter dom + 1, bestPath = min new (head $ bestPath dom) : bestPath dom, curPath = select dom new}

-- Complete solutions.
randomSearch :: Int -> Domain Path Path -> IO ()
randomSearch n dom = do
	best <- atomically $ newTVar 0
	forkIO $ logBest best >> return ()
	(p', _) <- init
	climb best init tweak select $ dom {curPath = p', bestPath = p'}
	return ()

	where

	init = do
		p' <- iterM (size dom) (swap $ size dom) $ curPath dom
		return (p', p')
	tweak = randomTweak $ 3 * size dom
	select = replace

ascent :: Int -> Domain Path [Path] -> IO ()
ascent n dom = do
	best <- atomically $ newTVar 0
	forkIO $ logBest best >> return ()
	climbR best n init tweak select restart dom' >> return () where

	init = let p = curPath dom in return (p, [p])
	tweak = bestNeighbour 200 $ size dom
	select = replace
	restart dom = iterM 5 (swap $ size dom) $ curPath dom
	dom' = dom {curIter = 0}

climbTest size initPath = do

	--m <- randomSearch 50 $ Domain {size = size, curIter = 0, maxIter = 1000, curPath = initPath, bestPath = initPath, logging = True}
	m <- ascent (size) $ Domain {size = size, curIter = 0, maxIter = 10000, curPath = initPath, bestPath = [initPath], logging = True}

	{-let	dom = Domain {size = size, curIter = 0, maxIter = 3 + 50 * size * size, curPath = p, bestPath = p, logging = True}
		init = return (p, p) :: IO (Path, Path)
		randomInit = do
			p' <- iterM size (swap size) p
			return (p', p')
		greedyInit = let g = greedy p in return (g, g) :: IO (Path, Path)

		--tweak = bestNeighbour 50 size
		tweak = progressiveBestNeighbour
		select = replace

	best <- atomically $ newTVar 0

	forkIO $ climb best init tweak select dom >> return ()

	putStrLn $ "climb (bestNeighbour 5) replace"
	logBest best -}
	--climb init tweak select dom mvwrite >> return ()


	-- wut mvwrite 0.0

	--m <- climb tweak select dom-}


	--m' <- liftM minimum $ replicateM 10 $ climb (iterM size (swap size) p) tweak select $ dom {logging = False}
	
	return ()
