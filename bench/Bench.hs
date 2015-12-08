{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where


import Prelude hiding (lookup)
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TM
import Criterion.Main
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM


type Key = Integer
type Content = Integer

buildTM :: Integer -> IO (TimeMap Key Content)
buildTM top = do
  x <- atomically $ TM.newTimeMap
  mapM_ (\(k,v) -> TM.insert k v x) $ [0..top] `zip` [0..top]
  return x

lookupTM :: Key -> TimeMap Key Content -> IO [Maybe Content]
lookupTM top x = atomically $ mapM (`TM.lookup` x) [0..top]

destroyTM :: Key -> TimeMap Key Content -> IO ()
destroyTM top x = atomically $ mapM_ (`TM.delete` x) [0..top]


main :: IO ()
main = do
  indiv10 <- buildTM 50
  indiv20 <- buildTM 50
  indiv30 <- buildTM 50
  indiv40 <- buildTM 50
  indiv50 <- buildTM 50

  batch10 <- buildTM 10
  batch20 <- buildTM 20
  batch30 <- buildTM 30
  batch40 <- buildTM 40
  batch50 <- buildTM 50

  threadDelay 1000000

  defaultMain
    [ bgroup "build"
      [ bench "10" $ whnfIO (buildTM 10)
      , bench "20" $ whnfIO (buildTM 20)
      , bench "30" $ whnfIO (buildTM 30)
      , bench "40" $ whnfIO (buildTM 40)
      , bench "50" $ whnfIO (buildTM 50)
      ]
    , bgroup "lookup"
      [ bench "10" $ whnfIO (lookupTM 10 indiv50)
      , bench "20" $ whnfIO (lookupTM 20 indiv50)
      , bench "30" $ whnfIO (lookupTM 30 indiv50)
      , bench "40" $ whnfIO (lookupTM 40 indiv50)
      , bench "50" $ whnfIO (lookupTM 50 indiv50)
      ]
    , bgroup "delete individual"
      [ bench "10" $ whnfIO (destroyTM 10 indiv10)
      , bench "20" $ whnfIO (destroyTM 20 indiv20)
      , bench "30" $ whnfIO (destroyTM 30 indiv30)
      , bench "40" $ whnfIO (destroyTM 40 indiv40)
      , bench "50" $ whnfIO (destroyTM 50 indiv50)
      ]
    , bgroup "delete batch"
      [ bench "10" $ whnfIO (TM.filterAgo 1 batch10)
      , bench "20" $ whnfIO (TM.filterAgo 1 batch20)
      , bench "30" $ whnfIO (TM.filterAgo 1 batch30)
      , bench "40" $ whnfIO (TM.filterAgo 1 batch40)
      , bench "50" $ whnfIO (TM.filterAgo 1 batch50)
      ]
    ]
