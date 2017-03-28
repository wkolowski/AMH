module Greedy where

import Problem

import Data.List

greedy :: Path -> Path
greedy (Path []) = error "LOLWUT empty list"
greedy (Path [v]) = Path [v]
greedy (Path (v:vs)) = case vs of
	[] -> Path []
	_ ->
		let	next = vs $> sortBy (\x y -> compare (dist v x) (dist v y)) $> head
			Path rest = greedy $ Path $ next : delete next vs
		in Path $ v : rest

main' = do
	(size, initPath) <- readInput

	let g = greedy initPath
	putStrLn $ show g
	putStrLn $ show $ pathLen g

--main = main'
