import Problem

import Data.Map (Map(..), insert, empty)

-- Fuel is the number of searches performed.
data Domain = Domain
	{ size :: Int
	, fuel :: Int
	, initPath :: Path
	, best :: Path
	}

-- Generate solutions randomly and return the best.
randomSearch :: Domain -> IO Path
randomSearch dom = do
	if fuel dom == 0
	then return $ best dom
	else do
		p <- iterM 20 (swap $ size dom) (initPath dom)
		if p <= best dom
		then randomSearch $ dom {fuel = fuel dom - 1, best = p}
		else randomSearch $ dom {fuel = fuel dom - 1}

coords :: Path -> Map Int (Float, Float)
coords (Path vs) = foldr (\(Vertex n x y) -> insert n (x, y)) empty vs


main = do
	(size, initPath) <- readInput

	let dom = Domain {size = size, initPath = initPath, fuel = 1000, best = initPath}

	m <- randomSearch dom

	putStrLn $ "Random search found path whose length is " ++ (show $ pathLen m)
