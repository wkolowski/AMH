module Threads where

import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryTakeMVar, putMVar, swapMVar)
import Control.Monad

sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)

printMsg :: String -> IO ()
printMsg name = for_ [1..3] $ \i -> do
	sleepMs 1
	putStrLn $ name ++ " number " ++ show i

wut = do
	forkIO $ printMsg "fork"
	forkIO $ printMsg "ninja"
	printMsg "main"

oldMain = do
	result <- newEmptyMVar

	forkIO (do
		sleepMs 5
		putStrLn "Calculated result!"
		putMVar result 42)

	putStrLn "Waiting..."
	value <- takeMVar result
	putStrLn ("The answer is: " ++ show value)
	value <- tryTakeMVar result
	case value of
		Nothing -> putStrLn "Nie ma :/"
		Just _ -> putStrLn "Jest!"

main = do

	mvwrite <- newEmptyMVar
	
	forkIO $ do
		sleepMs 3000
		putMVar mvwrite 97.5
		sleepMs 3000
		putMVar mvwrite 92.5
		sleepMs 3000
		putMVar mvwrite 88.89
	
	mvread <- newEmptyMVar
	putMVar mvread 100.0

	forever $ do
		write <- tryTakeMVar mvwrite
		case write of
			Nothing -> do
				read <- takeMVar mvread
				putStrLn $ show read
				putMVar mvread read
			Just write' -> do
				putStrLn $ show write'
				takeMVar mvread
				putMVar mvread write'
		sleepMs 1000
		


