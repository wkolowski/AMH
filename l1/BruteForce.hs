module BruteForce where

import Data.List
import Control.Monad

import TSP

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
