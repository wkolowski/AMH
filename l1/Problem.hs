module Problem
	( ($>)
	, Size(..)
	, Vertex(..)
	, Path(..)
	, dist
	, pathLen
	, readInput
	, swap
	, iterM
	)
where

import System.IO
import System.Random
import Control.Monad
import Data.List

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
