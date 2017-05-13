module Test where

import TSP
import HCArr

import Data.Array.MArray
import Data.Array.IO

import System.IO
import System.Random

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_correct :: ArrPath -> Property
prop_correct ap = monadicIO $ do
	(_, size) <- run $ getBounds ap
	i <- run $ randomRIO (2, size)
	j <- run $ randomRIO (2, size)
	len <- run $ arrPathLen ap
	diff <- run $ lenDiff ap i j
	run $ arrSwap ap i j
	len' <- run $ arrPathLen ap
	run $ putStrLn $ "i = " ++ (show i) ++ ", j = " ++ (show j) ++ ", len' = " ++ (show len') ++ ", len + diff = " ++ (show $ len + diff) ++ "len' - (len + diff) = " ++ (show $ len' - (len + diff))
	assert $ len' - (len + diff) < 0.2

main = do
	ap <- readArrPath
	quickCheckWith (stdArgs {maxSuccess = 5000}) (prop_correct ap)
