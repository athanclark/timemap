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
  x <- atomically $ TM.empty
  go x $ [0..top] `zip` [0..top]
  where
    go x [] = return x
    go x ((k,v):xs) = do
      x' <- TM.insert k v x
      go x' xs

destroyTM :: Integer -> TimeMap Key Content -> IO (TimeMap Key Content)
destroyTM top x = atomically $ go x [0..top]
  where
    go x [] = return x
    go x (k:ks) = do
      x' <- TM.delete k x
      go x' ks


main :: IO ()
main = do
  x10 <- buildTM 10
  x20 <- buildTM 20
  x30 <- buildTM 30
  x40 <- buildTM 40
  x50 <- buildTM 50
  threadDelay 1000000
  defaultMain
    [ bgroup "build"
      [ bench "10" $ whnfIO (buildTM 10)
      , bench "20" $ whnfIO (buildTM 20)
      , bench "30" $ whnfIO (buildTM 30)
      , bench "40" $ whnfIO (buildTM 40)
      , bench "50" $ whnfIO (buildTM 50)
      ]
    , bgroup "delete individual"
      [ bench "10" $ whnfIO (destroyTM 10 x50)
      , bench "20" $ whnfIO (destroyTM 20 x50)
      , bench "30" $ whnfIO (destroyTM 30 x50)
      , bench "40" $ whnfIO (destroyTM 40 x50)
      , bench "50" $ whnfIO (destroyTM 50 x50)
      ]
    , bgroup "delete batch"
      [ bench "10" $ whnfIO (TM.ago 60 x10)
      , bench "20" $ whnfIO (TM.ago 60 x20)
      , bench "30" $ whnfIO (TM.ago 60 x30)
      , bench "40" $ whnfIO (TM.ago 60 x40)
      , bench "50" $ whnfIO (TM.ago 60 x50)
      ]
    ]
