{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where


import Prelude hiding (lookup)
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TM
import Control.Concurrent (threadDelay)


type Key = Integer
type Content = Integer

buildTM :: Integer -> IO (TimeMap Key Content)
buildTM top = do
  x <- TM.newTimeMap
  mapM_ (\(k,v) -> TM.insert k v x) $ [0..top] `zip` [0..top]
  return x

destroyTM :: [Integer] -> TimeMap Key Content -> IO ()
destroyTM ds x = mapM_ (`TM.delete` x) ds


main :: IO ()
main = do
  x10 <- buildTM 1000
  x20 <- buildTM 2000
  x30 <- buildTM 3000
  x40 <- buildTM 4000
  x50 <- buildTM 5000

  threadDelay 500000

  fresh  <- buildTM 5000

  destroyTM [0..1000]    fresh
  destroyTM [1001..2000] fresh
  destroyTM [2001..3000] fresh
  destroyTM [3001..4000] fresh
  destroyTM [4001..5000] fresh

  threadDelay 1000000

  TM.filterAgo 1 x10
  TM.filterAgo 1 x20
  TM.filterAgo 1 x30
  TM.filterAgo 1 x40
  TM.filterAgo 1 x50

