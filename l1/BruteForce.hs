import Problem

import Data.List
import Control.Monad

pathPerms :: Path -> [Path]
pathPerms (Path []) = [Path []]
pathPerms (Path (x:xs)) = map Path $ map (x:) (permutations xs)

bruteForce :: Path -> Path
bruteForce p = minimum $ pathPerms p

bruteForce' :: Path -> IO Path
bruteForce' p = foldM (\p best -> if p < best && p /= best then do putStrLn $ "Found new best: " ++ (show $ pathLen p); return p else return best) p $ pathPerms p 

wut :: Int -> Path -> IO Path
wut n p
	| n <= 0 = return p
	| otherwise = do
		case dropWhile (>= p) $ pathPerms p of
			[] -> return p
			p' : _ -> wut (n - 1) p'

main = do
	(size, initPath) <- readInput
	let m = bruteForce initPath
	--putStrLn $ "Best solution is " ++ (show m) ++ ", its distance is " ++ (show $ pathLen m)

	p <- iterM 20 (swap size) initPath

	m' <- bruteForce' p
	putStrLn $ "Best solution is " ++ (show m') ++ ", its distance is " ++ (show $ pathLen m')

	forM_ [1..20] $ \k -> do
		m'' <- wut k p
		putStrLn $ "Got distance = " ++ (show $ pathLen m'')
