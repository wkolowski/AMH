module Main where

import TSP
import Greedy
import HCList
import HCArr

test_greedy :: IO ()
test_greedy = do
	(size, vs) <- readInput
	let g = greedy vs
	putStrLn $ "greedy: " ++ (show $ len g)

--main = test_greedyV

main = smallData
