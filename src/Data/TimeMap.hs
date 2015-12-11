{- |
Module      : Data.TimeMap
Copyright   : (c) 2015 Athan Clark

License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

A time-indexed mutable map for hashable keys.

The goal of this map is to provide moderately fast lookups and insertions for
key/value pairs, while implicitly keeping track of the last modification time of
each entity. The auxilliary time data is used for 'filterSince' and 'filterFromNow',
which quickly prune the data set to get rid of old entities.
-}

module Data.TimeMap
  ( -- * Types
    TimeMap
  , -- * Construction
    newTimeMap
  , insert
  , adjust
  , delete
  , -- * Query
    lookup
  , timeOf
  , ageOf
  , keys
  , elems
  , null
  , -- * Filter
    filter
  , filterWithKey
  , filterSince
  , filterFromNow
  ) where

import Prelude hiding (lookup, null, filter)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Hashable (Hashable (..))
import Data.Maybe (fromMaybe)
import qualified Data.Map                 as Map
import qualified Data.HashSet             as HS
import qualified Data.TimeMap.Internal    as MM
import qualified STMContainers.Map        as HT
import qualified Focus                    as F
import qualified ListT                    as L
import Control.Concurrent.STM


-- | A mutable reference for a time-indexed map, similar to a 'Data.STRef.STRef'.
data TimeMap k a = TimeMap
  { timeMap :: TVar (MM.MultiMap UTCTime k)
  , keysMap :: HT.Map k (UTCTime, a)
  }




-- | Create a fresh, empty map.
newTimeMap :: STM (TimeMap k a)
newTimeMap = TimeMap <$> newTVar MM.empty
                     <*> HT.new

-- | Inserts a key and value into a 'TimeMap' - it adds the value
--   or overwites an existing entity.
insert :: ( Hashable k
          , Eq k
          ) => k -> a -> TimeMap k a -> IO ()
insert k x xs = do
  now <- getCurrentTime
  atomically $ HT.focus (go now) k (keysMap xs)
  where
    go now mx = do
      modifyTVar (timeMap xs) $
        let changeOld = case mx of
                          Nothing          -> id
                          Just (oldTime,_) -> MM.remove oldTime k
        in MM.insert now k . changeOld
      pure ((), F.Replace (now, x))


-- | Performs a non-mutating lookup for some key.
lookup :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM (Maybe a)
lookup k xs = do
  mx <- HT.lookup k (keysMap xs)
  pure (snd <$> mx)


keys :: ( Hashable k
        , Eq k
        ) => TimeMap k a -> STM (HS.HashSet k)
keys xs = MM.elems <$> readTVar (timeMap xs)


elems :: TimeMap k a -> STM [a]
elems xs = L.toList $ (snd . snd) <$> HT.stream (keysMap xs)


null :: TimeMap k a -> STM Bool
null xs = HT.null (keysMap xs)

timeOf :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM (Maybe UTCTime)
timeOf k xs = do
  mx <- HT.lookup k (keysMap xs)
  pure (fst <$> mx)

ageOf :: ( Hashable k
         , Eq k
         ) => k -> TimeMap k a -> IO (Maybe NominalDiffTime)
ageOf k xs = do
  now <- getCurrentTime
  mt  <- atomically (timeOf k xs)
  pure (diffUTCTime now <$> mt)


-- | Adjusts the value at @k@, while updating its time.
adjust :: ( Hashable k
          , Eq k
          ) => (a -> a) -> k -> TimeMap k a -> IO ()
adjust f k xs = do
  now <- getCurrentTime
  atomically $ HT.focus (go now) k (keysMap xs)
  where
    go _ Nothing = pure ((), F.Keep)
    go now (Just (oldTime, y)) = do
      modifyTVar (timeMap xs) (MM.insert now k . MM.remove oldTime k)
      pure ((), F.Replace (now, f y))


-- | Deletes the value at @k@.
delete :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM ()
delete k xs = do
  HT.focus go k (keysMap xs)
  where
    go mx = do
      case mx of
        Nothing          -> pure ()
        Just (oldTime,_) -> modifyTVar' (timeMap xs) (MM.remove oldTime k)
      pure ((), F.Remove)



filter :: ( Hashable k
          , Eq k
          ) => (a -> Bool) -> TimeMap k a -> STM ()
filter p = filterWithKey (const p)


filterWithKey :: ( Hashable k
                 , Eq k
                 ) => (k -> a -> Bool) -> TimeMap k a -> STM ()
filterWithKey p xs = do
  ks <- (HS.toList . MM.elems) <$> readTVar (timeMap xs)
  mapM_ go ks
  where
    go k = HT.focus go' k (keysMap xs)
      where
        go' (Just (_,x)) | p k x     = pure ((), F.Keep)
                         | otherwise = pure ((), F.Remove)
        go' Nothing      = pure ((), F.Keep)


-- | Filters out all entries older than or equal to a designated time
filterSince :: ( Hashable k
               , Eq k
               ) => UTCTime
                 -> TimeMap k a
                 -> STM ()
filterSince t xs = do
  ts <- readTVar (timeMap xs)
  let (toCut, mx, result) = Map.splitLookup t ts
      found    = fromMaybe HS.empty mx
      toRemove = MM.elems toCut `HS.union` found
  writeTVar (timeMap xs) result
  mapM_ (\k -> HT.delete k $ keysMap xs) (HS.toList toRemove)


-- | Filters out all entries within some time frame
--
--   > filterFromNow 1 -- removes entities older than or equal to one second from now
filterFromNow :: ( Hashable k
                 , Eq k
                 ) => NominalDiffTime -- ^ Assumes a positive distance into the past
                   -> TimeMap k a
                   -> IO ()
filterFromNow t xs = do
  now <- getCurrentTime
  atomically $ filterSince (addUTCTime (negate t) now) xs
