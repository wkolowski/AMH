{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module TSP where

import Data.List
import Data.Random.Normal
import Data.Monoid

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
		initPath = text $> lines $> drop 1 $> map words $> map (\[x, y, z] -> Vertex {n = read x, x = read x, y = read y}) $> Path
	return $ (size, initPath)

readInputFromFile :: String -> IO (Size, Path)
readInputFromFile path = do
	text <- readFile path
	let	size = read $ (text $> lines) !! 0 :: Int
		initPath = text $> lines $> drop 1 $> map words $> map (\[x, y, z] -> Vertex {n = read x, x = read x, y = read y}) $> Path
	return (size, initPath)

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

iterM :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
iterM 0 _ = return
iterM 1 f = f
iterM n f = f >=> iterM (n - 1) f

-- Brute force solution.
pathPerms :: Path -> [Path]
pathPerms (Path []) = [Path []]
pathPerms (Path (x:xs)) = map Path $ map (x:) (permutations xs)

bruteForce :: Path -> Path
bruteForce p = minimum $ pathPerms p

bruteForce' :: Path -> IO Path
bruteForce' p = foldM (\p best -> if p < best && p /= best then do putStrLn $ "Found new best: " ++ (show $ pathLen p); return p else return best) p $ pathPerms p 

bruteForceTest :: IO ()
bruteForceTest = do
	(size, initPath) <- readInput
	let m = bruteForce initPath
	putStrLn $ "Best solution is " ++ (show m) ++ ", its distance is " ++ (show $ pathLen m)

-- Greedy solution.

greedy :: Path -> Path
greedy (Path []) = error "LOLWUT empty list"
greedy (Path [v]) = Path [v]
greedy (Path (v:vs)) = case vs of
	[] -> Path []
	_ ->
		let	!next = vs $> sortBy (\x y -> compare (dist v x) (dist v y)) $> head
			!(Path rest) = greedy $ Path $ next : delete next vs
		in Path $ v : rest

greedy2 :: Path -> Path
greedy2 (Path []) = error "LOLWUT empty list"
greedy2 (Path [v]) = Path [v]
greedy2 (Path (v:vs)) = Path $ v : rest where
	!next = vs $> sortBy (\x y -> compare (dist v x) (dist v y)) $> head
	!(Path rest) = greedy2 $ Path $ next : delete next vs

greedy3 :: Path -> Path
greedy3 (Path vs) = Path $ aux vs where
	aux [] = []
	aux [v] = [v]
	aux [v1, v2] = [v1, v2]
	aux (v:vs) = v : rest where
		!next = vs $> minimumBy (\x y -> compare (dist v x) (dist v y))
		!rest = aux (next : delete next vs)

greedyTest :: Size -> Path -> (Path -> Path) -> IO ()
greedyTest size initPath greedy = do
	let g = greedy initPath
	putStrLn $ "Greedy solution is " ++ show g
	putStrLn $ "Its distance is " ++ (show $ pathLen g)

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

climb :: TVar Float -> Init Path Path -> Tweak Path -> Select Path -> Domain Path Path -> IO Path
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

randomTweak :: Int -> Tweak Path
randomTweak numOfSwaps dom = iterM numOfSwaps (swap $ size dom) (curPath dom)

bestNeighbour :: Int -> Size -> Tweak Path
bestNeighbour times size dom = liftM minimum $ replicateM times $ swap size (curPath dom)

progressiveBestNeighbour :: Tweak Path
progressiveBestNeighbour dom = liftM minimum $ replicateM (50 + (curIter dom `div` 10)) $ swap (size dom) (curPath dom)

normalNeighbour :: Int -> Size -> Tweak Path
normalNeighbour times size dom = do
	let stddev = 1 / (fromIntegral size)
	r <- normalIO' (0.0, 1 + stddev) :: IO Float
	let n = floor $ 1 + 20 * abs r
	liftM minimum $ replicateM times $ swap size (curPath dom)

swapN :: Int -> Size -> Path -> IO Path
swapN n size p = iterM n (swap size) p

replace :: Select a
replace _ new = new

keep :: Select a
keep dom _ = curPath dom

climbR :: TVar Float -> Int -> Init Path [Path] -> Tweak Path -> Select Path -> Restart Path -> Domain Path [Path] -> IO Path
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

sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)

logBest :: TVar Float -> IO ThreadId
logBest best = forever $ do
	len <- atomically $ readTVar best
	putStrLn $ show len
	sleepMs 1000

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
	tweak = bestNeighbour 100 $ size dom
	select = replace
	restart dom = iterM 5 (swap $ size dom) $ curPath dom
	dom' = dom {curIter = 0}

climbTest = do
	(size, initPath) <- readInput

	let p = initPath

	--m <- randomSearch 50 $ Domain {size = size, curIter = 0, maxIter = 1000, curPath = initPath, bestPath = initPath, logging = True}
	m <- ascent size $ Domain {size = size, curIter = 0, maxIter = 1000, curPath = initPath, bestPath = [initPath], logging = True}

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

main = do
	(size, initPath) <- readInput
	putStrLn $ replicate 50 '=' ++ "greedy" ++ replicate 50 '='
	greedyTest size initPath greedy
	{-putStrLn $ replicate 50 '=' ++ "greedy2" ++ replicate 50 '='
	greedyTest size initPath greedy2
	putStrLn $ replicate 50 '=' ++ "greedy3" ++ replicate 50 '='
	greedyTest size initPath greedy3
	putStrLn $ replicate 50 '=' ++ "greedy4" ++ replicate 50 '='
	greedyTest size initPath greedy4
	putStrLn $ replicate 50 '=' ++ "greedy5" ++ replicate 50 '='
	greedyTest size initPath greedy5
	putStrLn $ replicate 50 '=' ++ "greedy6" ++ replicate 50 '='
	greedyTest size initPath greedy6-}
