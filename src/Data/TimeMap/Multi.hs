module Data.TimeMap.Multi where

import Data.TimeSet (TimeSet)
import qualified Data.TimeSet as TimeSet
import Data.Hashable (Hashable (..))
import Data.Time (UTCTime, NominalDiffTime)
import Control.Concurrent.STM (STM)
import qualified Data.HashSet             as HS


newtype TimeMultiMap k a = TimeMultiMap {getTimeMultiMap :: TimeSet (k,a)}


newTimeMultiMap :: STM (TimeMultiMap k a)
newTimeMultiMap = TimeMultiMap <$> TimeSet.newTimeSet


insert :: (Hashable k, Hashable a, Eq k, Eq a) => k -> a -> TimeMultiMap k a -> IO ()
insert k a (TimeMultiMap xs) = TimeSet.insert (k,a) xs


insertWithTime :: (Hashable k, Hashable a, Eq k, Eq a) => UTCTime -> k -> a -> TimeMultiMap k a -> STM ()
insertWithTime t k a (TimeMultiMap xs) = TimeSet.insertWithTime t (k,a) xs


elem :: (Hashable k, Hashable a, Eq k, Eq a) => k -> a -> TimeMultiMap k a -> STM Bool
elem k a (TimeMultiMap xs) = TimeSet.elem (k,a) xs


keys :: (Hashable k, Hashable a, Eq k, Eq a) => TimeMultiMap k a -> STM [k]
keys (TimeMultiMap xs) = (fmap fst . HS.toList) <$> TimeSet.elems xs


elems :: (Hashable k, Hashable a, Eq k, Eq a) => TimeMultiMap k a -> STM [a]
elems (TimeMultiMap xs) = (fmap snd . HS.toList) <$> TimeSet.elems xs


size :: TimeMultiMap k a -> STM Int
size (TimeMultiMap xs) = TimeSet.size xs


null :: TimeMultiMap k a -> STM Bool
null (TimeMultiMap xs) = TimeSet.null xs


timeOf :: (Hashable k, Hashable a, Eq k, Eq a) => k -> a -> TimeMultiMap k a -> STM (Maybe UTCTime)
timeOf k a (TimeMultiMap xs) = TimeSet.timeOf (k,a) xs


ageOf :: (Hashable k, Hashable a, Eq k, Eq a) => k -> a -> TimeMultiMap k a -> IO (Maybe NominalDiffTime)
ageOf k a (TimeMultiMap xs) = TimeSet.ageOf (k,a) xs


delete :: (Hashable k, Hashable a, Eq k, Eq a) => k -> a -> TimeMultiMap k a -> STM ()
delete k a (TimeMultiMap xs) = TimeSet.delete (k,a) xs


deleteAll :: (Hashable k, Hashable a, Eq k, Eq a) => k -> TimeMultiMap k a -> STM ()
deleteAll k t = do
  as <- elems t
  mapM_ (\a -> delete k a t) as


touch :: (Hashable k, Hashable a, Eq k, Eq a) => k -> a -> TimeMultiMap k a -> IO ()
touch k a (TimeMultiMap xs) = TimeSet.touch (k,a) xs


filter :: (Hashable k, Hashable a, Eq k, Eq a) => (k -> a -> Bool) -> TimeMultiMap k a -> STM ()
filter f (TimeMultiMap xs) = TimeSet.filter (\(k,a) -> f k a) xs


filterSince :: (Hashable k, Hashable a, Eq k, Eq a) => UTCTime -> TimeMultiMap k a -> STM ()
filterSince t (TimeMultiMap xs) = TimeSet.filterSince t xs


filterFromNow :: (Hashable k, Hashable a, Eq k, Eq a) => NominalDiffTime -> TimeMultiMap k a -> IO ()
filterFromNow t (TimeMultiMap xs) = TimeSet.filterFromNow t xs
