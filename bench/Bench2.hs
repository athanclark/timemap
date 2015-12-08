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

  threadDelay 1000000

  x50m10 <- destroyTM 1000 x50
  x50m20 <- destroyTM 2000 x50
  x50m30 <- destroyTM 3000 x50
  x50m40 <- destroyTM 4000 x50
  x50m50 <- destroyTM 5000 x50

  x50m10size <- atomically $ TM.size x50m10
  x50m20size <- atomically $ TM.size x50m20
  x50m30size <- atomically $ TM.size x50m30
  x50m40size <- atomically $ TM.size x50m40
  x50m50size <- atomically $ TM.size x50m50

  putStrLn $ "Size x50m10: " ++ show x50m10size
  putStrLn $ "Size x50m20: " ++ show x50m20size
  putStrLn $ "Size x50m30: " ++ show x50m30size
  putStrLn $ "Size x50m40: " ++ show x50m40size
  putStrLn $ "Size x50m50: " ++ show x50m50size

  threadDelay 1000000

  x10ago <- TM.ago 60 x10
  x20ago <- TM.ago 60 x20
  x30ago <- TM.ago 60 x30
  x40ago <- TM.ago 60 x40
  x50ago <- TM.ago 60 x50

  x10agosize <- atomically $ TM.size x10ago
  x20agosize <- atomically $ TM.size x20ago
  x30agosize <- atomically $ TM.size x30ago
  x40agosize <- atomically $ TM.size x40ago
  x50agosize <- atomically $ TM.size x50ago

  putStrLn $ "Size x10ago: " ++ show x10agosize
  putStrLn $ "Size x20ago: " ++ show x20agosize
  putStrLn $ "Size x30ago: " ++ show x30agosize
  putStrLn $ "Size x40ago: " ++ show x40agosize
  putStrLn $ "Size x50ago: " ++ show x50agosize
