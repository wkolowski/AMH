data Domain = Domain
	{ size :: Size
	, bests :: [Path]
	, paths :: [Path]
	, iter :: Int
	, maxIter :: Int
	, step :: [Path] -> IO [Path]
	, logging :: Bool
--	, numOfRestarts :: Int
	}

myStep :: Int -> Int -> Size -> Path -> IO [Path]
myStep times n size p = liftM (take n . sort) $ replicateM times $ swap size p
{-solve :: Int -> Int -> Size -> Path -> Path -> IO Path
solve numOfIter steps size p best
	| numOfIter <= 1 = return best
	| otherwise = do
		p' <- bestNeighbour steps size p
		let best' = min p' best
		when (numOfIter `mod` 10 == 0) $ putStrLn $ (show numOfIter) ++ " iterations to go, wutstep = " ++ (show steps) ++ ", total = " ++ (show $ numOfIter * steps) ++ ", distance = " ++ (show $ pathLen best')
		if numOfIter `mod` 25 == 0
		then solve (numOfIter - 1) (steps + 1) size p' (min p' best)
		else solve (numOfIter - 1) steps size p' best'

solve' :: Int -> Int -> Int -> Size -> [Path] -> Path -> IO [Path]
solve' numOfIter steps numOfTakes size ps best = do
	when (numOfIter `mod` 10 == 0) $
		putStrLn $ (show numOfIter) ++ " iterations to go, steps = " ++ (show steps) ++ ", numOfTakes = " ++ (show numOfTakes) ++
		", distance = " ++ (show $ pathLen (head ps)) ++ ", best = " ++ (show $ pathLen best)

	if numOfIter <= 0
	then return ps
	else do
		ps' <- map (bestNeighbour steps size) ps $> sequence $> liftM (take numOfTakes . sort)
		let best' = min best (head ps')
		
		solve' (numOfIter + 1) (100 + (numOfIter)) {-steps-} numOfTakes size ps' best' -- najlepsze: co 3, co 5 (dla 1000), dla 100: 
-}

newest :: ([Path] -> IO [Path]) -> [Path] -> Path -> IO [Path]
newest step ps best = do
	ps' <- step ps
	let best' = minimum $ best : ps'
	when (best /= best') $
		putStrLn $ "Found new best, its distance is " ++ (show $ pathLen best')
	newest step ps' best'

newest' :: Domain -> IO [Path]
newest' d = do
	ps <- step d $ paths d
	let best = head $ bests d
	let best' = minimum $ best : ps
	when (best /= best' && logging d) $
		putStrLn $ "Found new best, its distance is " ++ (show $ pathLen best')
	if iter d == maxIter d
	then do
		putStrLn $ "Exiting! Best: " ++ (show $ pathLen best')
		return $ [best']
	else newest' $ d {paths = ps, bests = best' : bests d, iter = iter d + 1}

restarts :: Int -> Path -> Domain -> IO [Path]
restarts n p d = liftM (take 1 . sort) $ replicateM n $ do
	p' <- iterM 10 (swap $ size d) p
	let d' = d {bests = [p'], paths = replicate n p'}
	liftM head $ newest' d

main = do
	(size, initPath) <- readInput

	--let iterstep = 10
	let fsize = fromInteger $ toInteger size

	let numOfIter = 50
	let steps = 20 + floor ((fsize * (fsize - 1) / 2) ** (2/10)) -- małe dane: 7/10
	let numOfTakes = 3 --10 + steps `div` 50
	
	{-putStrLn $ replicate 25 '=' ++ "solve'" ++ replicate 25 '='
	ps <- step steps numOfTakes size l -- best 100/10
	p <- liftM head $ solve' 1 {-numOfIter-} steps numOfTakes size ps (head ps)-}

	{-putStrLn $ replicate 25 '=' ++ "solve'" ++ replicate 25 '='
	l' <- iterM 20 (swap size) l
	p <- liftM head $ newest (\ps -> map (bestNeighbour steps size) ps $> sequence $> liftM (take numOfTakes . sort)) (replicate numOfTakes l') l'-}

	--putStrLn $ replicate 25 '=' ++ "solve'" ++ replicate 25 '='
	{-p <- iterM 10 (swap size) initPath
	let d = Domain {size = size, bests = [p], paths = replicate numOfTakes p, step = (\ps -> map (bestNeighbour steps size) ps $> sequence $> liftM (take numOfTakes . sort)), iter = 0, maxIter = 100, logging = False}-}
	--p <- liftM head $ newest' d


	{-putStrLn $ replicate 25 '=' ++ "restarts" ++ replicate 25 '='
	mins <- restarts 10 p d
	putStrLn $ replicate 25 '=' ++ "restarts" ++ replicate 25 '='
	putStrLn $ "The very best shit's distance is " ++ (show $ pathLen $ head mins)-}

	{-putStrLn $ replicate 25 '=' ++ "Trying to improve" ++ replicate 25 '='
	let p' = validPerms p $> take (10 * steps) $> minimum
	putStrLn $ "The result's distance is " ++ (show $ pathLen p')-}
	return ()
