{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Data.TimeMapSpec (spec) where

import           Data.TimeMap (TimeMap)
import qualified Data.TimeMap as TM

import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)


spec :: TestTree
spec = testGroup "Data.TimeMap"
  [ QC.testProperty "lookups should always return a result after insertion"
      lookupInsertExists
  , QC.testProperty "lookups should always fail after deletion"
      lookupDeleteNotExists
  , QC.testProperty "lookups should always fail after waiting past the time"
      lookupAgoNotExists
  ]


type Key = Integer
type Content = Integer


newtype BuiltTimeMap = BuiltTimeMap
  { getBuiltTimeMap :: [(Key, Content)]
  } deriving (Show, Arbitrary)

buildTimeMap :: BuiltTimeMap -> IO (TimeMap Key Content)
buildTimeMap xs = do
  x <- atomically TM.newTimeMap
  go (getBuiltTimeMap xs) x
  where
    go [] x = return x
    go ((k,v):vs) x = do
      TM.insert k v x
      go vs x


lookupInsertExists :: Key -> Content -> BuiltTimeMap -> Property
lookupInsertExists k v xs = ioProperty $ do
  x  <- buildTimeMap xs
  TM.insert k v x
  isJust <$> atomically (TM.lookup k x)


lookupDeleteNotExists :: Key -> BuiltTimeMap -> Property
lookupDeleteNotExists k xs = ioProperty $ do
  x <- buildTimeMap xs
  atomically $ do
    TM.delete k x
    isNothing <$> TM.lookup k x

lookupAgoNotExists :: Key -> Content -> BuiltTimeMap -> Property
lookupAgoNotExists k v xs = ioProperty $ do
  x <- buildTimeMap xs
  TM.insert k v x
  threadDelay 1000000
  1 `TM.ago` x
  isNothing <$> atomically (TM.lookup k x)
