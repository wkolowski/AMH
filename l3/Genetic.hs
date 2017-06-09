{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List
import Data.Monoid
import Data.Function
import Data.Array.MArray
import Data.Array.IO

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State

import System.IO
import System.Random

-- Forward composition.
($>) = flip ($)

type Size = Int
type Time = Int

data Vertex = Vertex
	{ n :: Int
	, x :: Double
	, y :: Double
	} deriving Eq

instance Show Vertex where
	show (Vertex n _ _) = show n

instance Read Vertex where
	readsPrec _ str =
		let	[n, x, y] = words str 
		in	[(Vertex (read n) (read x) (read y), "")]

genData :: Size -> FilePath -> IO ()
genData n path = withFile path WriteMode $ \handle -> do
	hPutStrLn handle (show n)
	forM_ [1..n] $ \k -> do
		x <- randomRIO (1.0, 100.0) :: IO Double
		y <- randomRIO (1.0, 100.0) :: IO Double
		hPutStrLn handle $ show k ++ " " ++ show x ++ " " ++ show y

parseInput :: String -> (Size, [Vertex], Time)
parseInput str =
	let	h:t = lines str
		size = read h
		vertices = map read $ init t
		time = read $ last t

	in	(size, vertices, time)

readInput :: IO (Size, [Vertex], Time)
readInput = liftM parseInput getContents

readInputFromFile' :: FilePath -> IO (Size, [Vertex], Time)
readInputFromFile' path = liftM parseInput (readFile path)

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

type Path = IOArray Int Vertex

mkPath :: Size -> [Vertex] -> IO Path
mkPath size vs
	| size <= 0 = error "Size can't be less than 0."
	| vs == [] = error "There must be some vertices"
	| otherwise = newListArray (1, size) vs

pathLen :: Path -> IO Double
pathLen ap = do
	(lo, hi) <- getBounds ap
	vs <- mapM (\i -> readArray ap i) [lo..hi]
	return $ len vs 

readPath :: IO Path
readPath = do
	(size, vs, _) <- readInput
	mkPath size vs

greedyPath :: IO Path
greedyPath = do
	(size, vs, _) <- readInput
	mkPath size $ greedy vs

getSize :: Path -> IO Size
getSize ap = liftM snd $ getBounds ap

swap :: Path -> Int -> Int -> IO ()
swap ap i j = do
	pi <- readArray ap i
	pj <- readArray ap j
	writeArray ap i pj
	writeArray ap j pi

randSwap :: Path -> IO ()
randSwap ap = do
	(lo, hi) <- getBounds ap
	i <- randomRIO (lo + 1, hi)
	j <- randomRIO (lo + 1, hi)
	swap ap i j

randomize :: Path -> IO ()
randomize ap = do
	size <- getSize ap
	replicateM size $ randSwap ap
	return ()

randomPath :: IO Path
randomPath = do
	ap <- readPath
	randomize ap
	return ap

lenDiff :: Path -> Int -> Int -> IO Double
lenDiff ap i' j' = do
	let	i = min i' j'
		j = max i' j'

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

copy :: Path -> Path -> IO ()
copy from to = do
	(lo, hi) <- getBounds from
	forM_ [lo..hi] $ \i -> do
		from_i <- readArray from i
		writeArray to i from_i

printArr :: Path -> IO ()
printArr p = do
	size <- getSize p
	forM_ [1..size] $ \i -> do
		v <- readArray p i
		hPutStr stderr $ show v ++ " "

	v1 <- readArray p 1
	hPutStrLn stderr $ show v1

data EvolutionState = EvolutionState
	{ size :: Size
	, time :: Time
	, population :: [Path]
	, best :: Path
	, bestLen :: Double
	}

generateSamples :: Path -> Int -> StateT EvolutionState IO [(Int, Int)]
generateSamples ap n = do
	(lo, hi) <- lift $ getBounds ap
	lift $ aux lo hi n []

	where

	aux lo hi 0 acc = return acc
	aux lo hi n acc = do
		i <- randomRIO (lo + 1, hi)
		j <- randomRIO (lo + 1, hi)
		if i == j
		then aux lo hi n acc
		else aux lo hi (n - 1) $ (i, j) : acc

minSample :: Path -> [(Int, Int)] -> StateT EvolutionState IO (Int, Int, Double)
minSample ap samples = do
	samples' <- lift $ sequence $ map (\(i, j) -> do
		l <- lenDiff ap i j
		return (i, j, l)) samples
	return $ minimumBy (\(_, _, l) (_, _, l') -> compare l l') samples'

mutate :: Int -> Path -> StateT EvolutionState IO ()
mutate n p = do
	samples <- generateSamples p n
	(i, j, _) <- minSample p samples
	lift $ swap p i j

type Mutate = Path -> StateT EvolutionState IO ()

evolve :: Int -> Mutate -> IO Double
evolve populationSize mutate = do
	(size, vertices, time) <- readInput

	greedyPath <- mkPath size vertices -- (greedy vertices)
	otherPaths <- replicateM (populationSize - 1) (mkPath size vertices)
	mapM_ randomize otherPaths

	bestLen <- pathLen greedyPath

	evalStateT aux $ EvolutionState	{ size = size
					, time = time
					, population = greedyPath : otherPaths
					, best = greedyPath
					, bestLen = bestLen
	}

	where

	aux :: StateT EvolutionState IO Double
	aux = do
		st <- get

		l <- lift $ forM (population st) $ \path -> do
			len <- pathLen path
			return (path, len)

		let l'@((p, len):_) = l $> sortBy (compare `on` snd)

		when (len < bestLen st) $ do
			modify $ \st -> st {best = p, bestLen = len}
			--lift $ printArr p

		let	bests = take (populationSize `div` 2) $ map fst l'
			worsts = drop (populationSize `div` 2) $ map fst l'
	
		lift $ putStrLn $ "Second best length is " ++ show (snd (head l'))

		lift $ zipWithM copy bests worsts

		mapM_ mutate worsts

		{-lift $ forM_ bests $ \path -> do
			p <- randomRIO (0.0, 1.0) :: IO Double
			when (p < 0.0001) (randomize path)-}

		modify $ \st -> st {population = bests ++ worsts}

		aux

		return 42.0

evolve2 :: Int -> Mutate -> IO Double
evolve2 populationSize mutate = do
	(size, vertices, time) <- readInput

	greedyPath <- mkPath size vertices -- (greedy vertices)
	otherPaths <- replicateM (populationSize - 1) (mkPath size vertices)
	mapM_ randomize otherPaths

	bestLen <- pathLen greedyPath

	evalStateT aux $ EvolutionState	{ size = size
					, time = time
					, population = greedyPath : otherPaths
					, best = greedyPath
					, bestLen = bestLen
	}

	where

	aux :: StateT EvolutionState IO Double
	aux = do
		st <- get

		l <- lift $ forM (population st) $ \path -> do
			len <- pathLen path
			return (path, len)

		let l'@((p, len):_) = l $> sortBy (compare `on` snd)

		when (len < bestLen st) $ do
			modify $ \st -> st {best = p, bestLen = len}
			--lift $ printArr p

		{-let	bests = take (8 * populationSize `div` 10) $ map fst l'
			preworsts = take (populationSize `div` 10) $ drop (8 * populationSize `div` 10) $ map fst l'
			worsts = take (populationSize `div` 10) $ drop (9 * populationSize `div` 10) $ map fst l'-}

		let (bests, (middle, worsts)) = fmap (splitAt (8 * populationSize `div` 10)) $ splitAt (populationSize `div` 10) $ map fst l'
	
		lift $ putStrLn $ "Best length is " ++ show (snd (head l'))

		lift $ zipWithM copy bests worsts

		mapM_ mutate worsts

		let newPopulation = bests ++ (middle ++ worsts)

		{-lift $ forM_ bests $ \path -> do
			p <- randomRIO (0.0, 1.0) :: IO Double
			when (p < 0.0001) (randomize path)-}

		modify $ \st -> st {population = newPopulation}

		aux

		return 42.0


main = evolve2 10000 (mutate 100)

-- berlin52 10000 10 wynik: 7884


	
