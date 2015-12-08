{-# LANGUAGE
    OverloadedStrings
  #-}

module Main where


import Prelude hiding (lookup)
import Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TM
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM


type Key = Integer
type Content = Integer

buildTM :: Integer -> IO (TimeMap Key Content)
buildTM top = do
  x <- atomically $ TM.newTimeMap
  mapM_ (\(k,v) -> TM.insert k v x) $ [0..top] `zip` [0..top]
  return x

destroyTM :: [Integer] -> TimeMap Key Content -> IO ()
destroyTM ds x = atomically $ mapM_ (`TM.delete` x) ds


main :: IO ()
main = do
  x10 <- buildTM 1000
  x20 <- buildTM 2000
  x30 <- buildTM 3000
  x40 <- buildTM 4000
  x50 <- buildTM 5000

  x10size <- atomically $ TM.size x10
  x20size <- atomically $ TM.size x20
  x30size <- atomically $ TM.size x30
  x40size <- atomically $ TM.size x40
  x50size <- atomically $ TM.size x50

  putStrLn $ "Size x10: " ++ show x10size
  putStrLn $ "Size x20: " ++ show x20size
  putStrLn $ "Size x30: " ++ show x30size
  putStrLn $ "Size x40: " ++ show x40size
  putStrLn $ "Size x50: " ++ show x50size

  threadDelay 500000

  fresh  <- buildTM 5000

  destroyTM [0..1000]    fresh
  x50m10size <- atomically $ TM.size fresh
  destroyTM [1001..2000] fresh
  x50m20size <- atomically $ TM.size fresh
  destroyTM [2001..3000] fresh
  x50m30size <- atomically $ TM.size fresh
  destroyTM [3001..4000] fresh
  x50m40size <- atomically $ TM.size fresh
  destroyTM [4001..5000] fresh
  x50m50size <- atomically $ TM.size fresh

  putStrLn $ "Size x50m10: " ++ show x50m10size
  putStrLn $ "Size x50m20: " ++ show x50m20size
  putStrLn $ "Size x50m30: " ++ show x50m30size
  putStrLn $ "Size x50m40: " ++ show x50m40size
  putStrLn $ "Size x50m50: " ++ show x50m50size

  threadDelay 1000000

  TM.filterAgo 1 x10
  TM.filterAgo 1 x20
  TM.filterAgo 1 x30
  TM.filterAgo 1 x40
  TM.filterAgo 1 x50

  x10agosize <- atomically $ TM.size x10
  x20agosize <- atomically $ TM.size x20
  x30agosize <- atomically $ TM.size x30
  x40agosize <- atomically $ TM.size x40
  x50agosize <- atomically $ TM.size x50

  putStrLn $ "Size x10ago: " ++ show x10agosize
  putStrLn $ "Size x20ago: " ++ show x20agosize
  putStrLn $ "Size x30ago: " ++ show x30agosize
  putStrLn $ "Size x40ago: " ++ show x40agosize
  putStrLn $ "Size x50ago: " ++ show x50agosize
