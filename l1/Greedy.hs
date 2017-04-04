module Greedy where

import Data.List

import TSP

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
