module Data.TimeSet where

import Data.TimeMap (TimeMap, newTimeMap)
import qualified Data.TimeMap as TimeMap
import Data.Hashable (Hashable (..))
import Data.Time (UTCTime, NominalDiffTime)
import qualified Data.HashSet             as HS
import Control.Concurrent.STM (STM)


newtype TimeSet a = TimeSet {getTimeSet :: TimeMap a ()}


newTimeSet :: STM (TimeSet a)
newTimeSet = TimeSet <$> newTimeMap


insert :: (Hashable a, Eq a) => a -> TimeSet a -> IO ()
insert a (TimeSet xs) = TimeMap.insert a () xs


insertWithTime :: (Hashable a, Eq a) => UTCTime -> a -> TimeSet a -> STM ()
insertWithTime t a (TimeSet xs) = TimeMap.insertWithTime t a () xs


elem :: (Hashable a, Eq a) => a -> TimeSet a -> STM Bool
elem a (TimeSet xs) = do
  r <- TimeMap.lookup a xs
  case r of
    Nothing -> pure False
    Just () -> pure True


elems :: (Hashable a, Eq a) => TimeSet a -> STM (HS.HashSet a)
elems (TimeSet xs) = TimeMap.keys xs


size :: TimeSet a -> STM Int
size (TimeSet xs) = TimeMap.size xs


null :: TimeSet a -> STM Bool
null (TimeSet xs) = TimeMap.null xs


timeOf :: (Hashable a, Eq a) => a -> TimeSet a -> STM (Maybe UTCTime)
timeOf a (TimeSet xs) = TimeMap.timeOf a xs


ageOf :: (Hashable a, Eq a) => a -> TimeSet a -> IO (Maybe NominalDiffTime)
ageOf a (TimeSet xs) = TimeMap.ageOf a xs


delete :: (Hashable a, Eq a) => a -> TimeSet a -> STM ()
delete a (TimeSet xs) = TimeMap.delete a xs


touch :: (Hashable a, Eq a) => a -> TimeSet a -> IO ()
touch a (TimeSet xs) = TimeMap.touch a xs


filter :: (Hashable a, Eq a) => (a -> Bool) -> TimeSet a -> STM ()
filter f (TimeSet xs) = TimeMap.filterWithKey (\k _ -> f k) xs


filterSince :: (Hashable a, Eq a) => UTCTime -> TimeSet a -> STM ()
filterSince t (TimeSet xs) = TimeMap.filterSince t xs


filterFromNow :: (Hashable a, Eq a) => NominalDiffTime -> TimeSet a -> IO ()
filterFromNow t (TimeSet xs) = TimeMap.filterFromNow t xs
