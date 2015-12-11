{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where


import Prelude hiding (lookup)
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TM
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)


type Key = Integer
type Content = Integer

buildTM :: Integer -> IO (TimeMap Key Content)
buildTM top = do
  x <- atomically TM.newTimeMap
  mapM_ (\(k,v) -> TM.insert k v x) $ [0..top] `zip` [0..top]
  return x

destroyTM :: [Integer] -> TimeMap Key Content -> IO ()
destroyTM ds x = atomically $ mapM_ (`TM.delete` x) ds


main :: IO ()
main = do
  xs <- buildTM 5000

  threadDelay 500000

  fresh  <- buildTM 5000

  destroyTM [0..5000]    fresh

  threadDelay 500000

  TM.filterFromNow 1 xs

