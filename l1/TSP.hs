{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module TSP where

import Data.List
import Data.Monoid
import Data.Random.Normal
import Data.Array.MArray
import Data.Array.IO

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import System.IO
import System.Random

-- Forward composition.
($>) = flip ($)

type Size = Int

data Vertex = Vertex
	{ n :: Int
	, x :: Float
	, y :: Float
	} deriving Eq

newtype Path = Path [Vertex] --deriving Eq

instance Show Vertex where
	show (Vertex n _ _) = show n

instance Show Path where
	show (Path vs) = show vs

-- Euclidean distance between two vertices.
dist :: Vertex -> Vertex -> Float
dist (Vertex _ x1 y1) (Vertex _ x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Path length (sum of distances from each point to the next plus the distance
-- from the last point to the first).
pathLen :: Path -> Float
pathLen (Path []) = 0.0
pathLen (Path [_]) = 0.0
pathLen (Path vs) = dist (head vs) (last vs) + pathLen' vs
	where	pathLen' [] = 0.0
		pathLen' [_] = 0.0
		pathLen' (v1:v2:vs) = dist v1 v2 + pathLen' (v2:vs)

instance Eq Path where
	p == p' = pathLen p == pathLen p'

-- Paths are ordered by their length.
instance Ord Path where
	compare vs1 vs2 = compare (pathLen vs1) (pathLen vs2)

genData :: Size -> FilePath -> IO ()
genData n path = withFile path WriteMode $ \handle -> do
	hPutStrLn handle (show n)
	forM_ [1..n] $ \k -> do
		x <- randomRIO (1.0, 100.0) :: IO Float
		y <- randomRIO (1.0, 100.0) :: IO Float
		hPutStrLn handle $ show k ++ " " ++ show x ++ " " ++ show y

readInput :: IO (Size, Path)
readInput = do
	text <- getContents
	let	size = read $ (text $> lines) !! 0 :: Int
		initPath = text $> lines $> drop 1 $> map words $> map (\[n, x, y] -> Vertex {n = read n, x = read x, y = read y}) $> Path
	return $ (size, initPath)

readInputFromFile :: String -> IO (Size, Path)
readInputFromFile path = do
	text <- readFile path
	let	size = read $ (text $> lines) !! 0 :: Int
		initPath = text $> lines $> drop 1 $> map words $> map (\[n, x, y] -> Vertex {n = read n, x = read x, y = read y}) $> Path
	return (size, initPath)

iterM :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
iterM 0 _ = return
iterM 1 f = f
iterM n f = f >=> iterM (n - 1) f

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

sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)

logBest :: TVar Float -> IO ()
logBest best = forever $ do
	len <- atomically $ readTVar best
	putStrLn $ show len
	sleepMs 1000
	return ()
