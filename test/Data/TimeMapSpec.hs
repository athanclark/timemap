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

import Control.Concurrent.STM
import Control.Concurrent (threadDelay)


spec :: TestTree
spec = testGroup "Data.TimeMap"
  [ QC.testProperty "lookups should always succeed after insertion"
      lookupInsertExists
  , QC.testProperty "lookups should always fail after deletion"
      lookupDeleteNotExists
  , QC.testProperty "lookups should always fail after waiting past the time"
      lookupAgoNotExists
  , QC.testProperty "lookups should always succeed after waiting before the time"
      lookupAgoExists
  ]


type Key = Integer
type Content = Integer


newtype BuiltTimeMap = BuiltTimeMap
  { getBuiltTimeMap :: [(Key, Content)]
  } deriving (Show, Arbitrary)

buildTimeMap :: BuiltTimeMap -> IO (TimeMap Key Content)
buildTimeMap xs = do
  x <- atomically $ TM.newTimeMap
  mapM_ (\(k,v) -> TM.insert k v x) $ getBuiltTimeMap xs
  return x


lookupInsertExists :: Key -> Content -> BuiltTimeMap -> Property
lookupInsertExists k v xs = ioProperty $ do
  x  <- buildTimeMap xs
  TM.insert k v x
  isJust <$> atomically (TM.lookup k x)


lookupDeleteNotExists :: Key -> BuiltTimeMap -> Property
lookupDeleteNotExists k xs = ioProperty $ do
  x <- buildTimeMap xs
  atomically (TM.delete k x)
  isNothing <$> atomically (TM.lookup k x)

lookupAgoNotExists :: Key -> Content -> BuiltTimeMap -> Property
lookupAgoNotExists k v xs = ioProperty $ do
  x <- buildTimeMap xs
  TM.insert k v x
  threadDelay 1000000
  TM.filterFromNow 1 x
  isNothing <$> atomically (TM.lookup k x)

lookupAgoExists :: Key -> Content -> BuiltTimeMap -> Property
lookupAgoExists k v xs = ioProperty $ do
  x <- buildTimeMap xs
  TM.insert k v x
  threadDelay 500000
  TM.filterFromNow 1 x
  isJust <$> atomically (TM.lookup k x)
