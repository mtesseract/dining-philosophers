{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude          hiding (forM_)
import           Control.Concurrent.STM (retry)
import           Control.Monad          (forM_)
import           Prelude                ((!!))
import           System.Exit
import           System.Random

newtype Fork = Fork Int deriving (Show)

n :: Int
n = 5

main :: IO ()
main = do
  forks <- mapM (newTMVarIO . Fork) [1..n]
  forM_ [1..n] $ \i -> async (philosopher forks i)
  threadDelay (10 * 10^6)

forkAvailable :: TMVar Fork -> STM Bool
forkAvailable forkTV = not <$> isEmptyTMVar forkTV

philosopher :: [TMVar Fork] -> Int -> IO ()
philosopher forks i = forever $ do
  (leftFork, rightFork) <- atomically $ do
    leftReady  <- forkAvailable leftForkTV
    rightReady <- forkAvailable rightForkTV
    if leftReady && rightReady
      then (,) <$> takeTMVar leftForkTV <*> takeTMVar rightForkTV
      else retry
  say $ "[Philosopher " ++ tshow i ++ "] Eating with forks " ++ tshow (leftForkIdx, rightForkIdx)
  round . (10^6 *) <$> randomRIO (0 :: Double, 1 :: Double) >>= threadDelay
  atomically $ do
    putTMVar leftForkTV leftFork
    putTMVar rightForkTV rightFork
  round . (10^5 *) <$> randomRIO (0 :: Double, 1 :: Double) >>= threadDelay

  where nForks = length forks
        leftForkIdx = i - 1 `mod` nForks
        rightForkIdx = i `mod` nForks
        leftForkTV = forks !! leftForkIdx
        rightForkTV = forks !! rightForkIdx
