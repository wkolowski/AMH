{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Data.List
import Data.Monoid
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

parseVertices :: String -> (Size, [Vertex])
parseVertices str =
	let	h:t = lines str
		size = read h
		vertices = map read t
	in	(size, vertices)

readVertices :: IO (Size, [Vertex])
readVertices = liftM parseVertices getContents

readVerticesFromFile' :: FilePath -> IO (Size, [Vertex])
readVerticesFromFile' path = liftM parseVertices (readFile path)

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
	(size, vs) <- readVertices
	mkPath size vs

greedyPath :: IO Path
greedyPath = do
	(size, vs) <- readVertices
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

data AnnealingState = AnnealingState
	{ size :: Size
	, temperature :: Double
	, mulRate :: Double
	, subRate :: Double
	, cur :: Path
	, curLen :: Double
	, bestLen :: Double
	}

generateSamples :: Path -> Int -> StateT AnnealingState IO [(Int, Int)]
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

minSample :: Path -> [(Int, Int)] -> StateT AnnealingState IO (Int, Int, Double)
minSample ap samples = do
	samples' <- lift $ sequence $ map (\(i, j) -> do
		l <- lenDiff ap i j
		return (i, j, l)) samples
	return $ minimumBy (\(_, _, l) (_, _, l') -> compare l l') samples'

simpleTweak :: Int -> StateT AnnealingState IO (Int, Int, Double)
simpleTweak numOfSamples = do
	p <- liftM cur get
	samples <- generateSamples p numOfSamples
	minSample p samples

printArr :: Path -> IO ()
printArr p = do
	size <- getSize p
	forM_ [1..size] $ \i -> do
		v <- readArray p i
		hPutStr stderr $ show v ++ " "

	v1 <- readArray p 1
	hPutStrLn stderr $ show v1

anneal :: IO Double
anneal = do
	initPath <- greedyPath
	pathSize <- getSize initPath
	initLen <- pathLen initPath

	printArr initPath

	evalStateT aux $ AnnealingState { size = pathSize
					, temperature = initLen
					, mulRate = 1 - 1e-6
					, subRate = 1e-3 + initLen / 10^6
					, cur = initPath
					, curLen = initLen
					, bestLen = initLen
					}
	where

	aux :: StateT AnnealingState IO Double
	aux = do
		st <- get
		if temperature st <= 0.0
		then return $ bestLen st
		else do
			[i, j] <- lift $ replicateM 2 (randomRIO (2, size st))
			diff <- lift $ lenDiff (cur st) i j

			if curLen st + diff < bestLen st
			then do
				lift $ swap (cur st) i j
				modify $ \st -> st {curLen = curLen st + diff, bestLen = curLen st + diff}
				lift $ printArr (cur st)
			else do
				x <- lift $ randomRIO (0.0, 1.0)
				when (exp (diff / temperature st) < x) $ do
					lift $ swap (cur st) i j
					modify $ \st -> st {curLen = curLen st + diff}

			modify $ \st -> st {temperature = mulRate st * temperature st - subRate st}
			aux

main = anneal >>= putStrLn . show
