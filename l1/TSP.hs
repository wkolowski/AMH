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
	, x :: Double
	, y :: Double
	} deriving Eq

instance Show Vertex where
	show (Vertex n _ _) = show n

genData :: Size -> FilePath -> IO ()
genData n path = withFile path WriteMode $ \handle -> do
	hPutStrLn handle (show n)
	forM_ [1..n] $ \k -> do
		x <- randomRIO (1.0, 100.0) :: IO Double
		y <- randomRIO (1.0, 100.0) :: IO Double
		hPutStrLn handle $ show k ++ " " ++ show x ++ " " ++ show y

parseInput :: String -> (Size, [Vertex])
parseInput str = 
	let	size = read $ (str $> lines) !! 0 :: Int
		vertices = str $> lines $> drop 1 $> map words $> map (\[n, x, y] -> Vertex {n = read n, x = read x, y = read y})
	in (size, vertices)

readInput :: IO (Size, [Vertex])
readInput = do
	str <- getContents
	return $ parseInput str

readInputFromFile :: String -> IO (Size, [Vertex])
readInputFromFile path = do
	str <- readFile path
	return $ parseInput str

-- Euclidean distance between two vertices.
dist :: Vertex -> Vertex -> Double
dist (Vertex _ x1 y1) (Vertex _ x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Path length (sum of distances from each point to the next plus the distance
-- from the last point to the first).
len :: [Vertex] -> Double
len [] = 0.0
len [_] = 0.0
len vs = dist (head vs) (last vs) + pathLen' vs
	where	pathLen' [] = 0.0
		pathLen' [_] = 0.0
		pathLen' (v1:v2:vs) = dist v1 v2 + pathLen' (v2:vs)

greedy :: [Vertex] -> [Vertex]
greedy [] = []
greedy [v] = [v]
greedy (v:vs) = v : rest where
	!next = vs $> minimumBy (\x y -> compare (dist v x) (dist v y))
	!rest = greedy $ next : delete next vs

sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)

logBest :: TVar Double -> IO ()
logBest best = forever $ do
	len <- atomically $ readTVar best
	putStrLn $ show len
	sleepMs 1000
	return ()

iterM :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
iterM 0 _ = return
iterM 1 f = f
iterM n f = f >=> iterM (n - 1) f
