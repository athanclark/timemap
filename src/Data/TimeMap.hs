{-# LANGUAGE
    BangPatterns
  #-}

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
  , insertWithTime
  , update
  , updateWithTime
  , adjust
  , adjustWithTime
  , delete
  , touch
  , -- * Query
    lookup
  , timeOf
  , ageOf
  , keys
  , elems
  , toList
  , size
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
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict          as Map
import qualified Data.HashSet             as HS
import qualified Data.TimeMap.Internal    as MM
import qualified STMContainers.Map        as HT
import qualified Focus                    as F
import qualified ListT                    as L
import Control.Monad (forM)
import Control.Concurrent.STM (STM, atomically, TVar, writeTVar, readTVar, modifyTVar', modifyTVar, newTVar)


data TimeIndexed a = TimeIndexed
  { indexedTime  :: {-# UNPACK #-} !UTCTime
  , indexedValue :: a
  }


-- | A mutable reference for a time-indexed map, similar to a 'Data.STRef.STRef'.
data TimeMap k a = TimeMap
  { timeMap :: !(TVar (MM.MultiMap UTCTime k))
  , keysMap :: !(HT.Map k (TimeIndexed a))
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
  atomically $ insertWithTime now k x xs

{-# INLINEABLE insert #-}

insertWithTime :: ( Hashable k
                  , Eq k
                  ) => UTCTime -> k -> a -> TimeMap k a -> STM ()
insertWithTime now k x xs =
  HT.focus go k (keysMap xs)
  where
    go mx = do
      modifyTVar (timeMap xs) $
        let changeOld = case mx of
                          Nothing -> id
                          Just (TimeIndexed oldTime _) ->
                            MM.remove oldTime k
        in  MM.insert now k . changeOld
      pure ((), F.Replace (TimeIndexed now x))

{-# INLINEABLE insertWithTime #-}

-- | Performs a non-mutating lookup for some key.
lookup :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM (Maybe a)
lookup k xs =
  (\mx' -> indexedValue <$> mx') <$> HT.lookup k (keysMap xs)

{-# INLINEABLE lookup #-}

keys :: ( Hashable k
        , Eq k
        ) => TimeMap k a -> STM (HS.HashSet k)
keys xs = MM.elems <$> readTVar (timeMap xs)

{-# INLINEABLE keys #-}

elems :: TimeMap k a -> STM [a]
elems xs = L.toList $ (indexedValue . snd) <$> HT.stream (keysMap xs)

toList :: ( Hashable k
          , Eq k
          ) => TimeMap k a -> STM [(k, a)]
toList xs = do
  keys' <- (HS.toList . MM.elems) <$> readTVar (timeMap xs)
  forM keys' $ \k -> do
    mVal <- HT.lookup k (keysMap xs)
    pure (k, indexedValue (fromJust mVal))

size :: TimeMap k a -> STM Int
size xs = length <$> elems xs

null :: TimeMap k a -> STM Bool
null xs = HT.null (keysMap xs)

timeOf :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM (Maybe UTCTime)
timeOf k xs = do
  mx <- HT.lookup k (keysMap xs)
  pure $! indexedTime <$> mx

{-# INLINEABLE timeOf #-}

ageOf :: ( Hashable k
         , Eq k
         ) => k -> TimeMap k a -> IO (Maybe NominalDiffTime)
ageOf k xs = do
  now <- getCurrentTime
  mt  <- atomically $! timeOf k xs
  pure $! diffUTCTime now <$> mt

{-# INLINEABLE ageOf #-}

-- | Updates or deletes the value at @k@, while updating its time.
update :: ( Hashable k
          , Eq k
          ) => (a -> Maybe a) -> k -> TimeMap k a -> IO ()
update p k xs = do
  now <- getCurrentTime
  atomically $ updateWithTime now p k xs

{-# INLINEABLE update #-}

updateWithTime :: ( Hashable k
                  , Eq k
                  ) => UTCTime -> (a -> Maybe a) -> k -> TimeMap k a -> STM ()
updateWithTime now p k xs =
  HT.focus go k (keysMap xs)
  where
    go Nothing = pure ((), F.Keep)
    go (Just (TimeIndexed oldTime y)) =
      let (action,minsert) =
            case p y of
              Nothing -> (F.Remove                      , MM.remove oldTime k)
              Just y' -> (F.Replace (TimeIndexed now y'), id)
      in do modifyTVar (timeMap xs) (MM.insert now k . minsert)
            pure ((), action)

{-# INLINEABLE updateWithTime #-}


-- | Adjusts the value at @k@, while updating its time.
adjust :: ( Hashable k
          , Eq k
          ) => (a -> a) -> k -> TimeMap k a -> IO ()
adjust f k xs = do
  now <- getCurrentTime
  atomically $ adjustWithTime now f k xs

{-# INLINEABLE adjust #-}

adjustWithTime :: ( Hashable k
                  , Eq k
                  ) => UTCTime -> (a -> a) -> k -> TimeMap k a -> STM ()
adjustWithTime now f k xs =
  HT.focus go k (keysMap xs)
  where
    go Nothing = pure ((), F.Keep)
    go (Just (TimeIndexed oldTime y)) = do
      modifyTVar (timeMap xs) (MM.insert now k . MM.remove oldTime k)
      pure ((), F.Replace (TimeIndexed now $! f y))

{-# INLINEABLE adjustWithTime #-}


-- | Deletes the value at @k@.
delete :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM ()
delete k xs = HT.focus go k (keysMap xs)
  where
    go mx = do
      case mx of
        Nothing -> pure ()
        Just (TimeIndexed oldTime _) ->
          modifyTVar' (timeMap xs) (MM.remove oldTime k)
      pure ((), F.Remove)

{-# INLINEABLE delete #-}


-- | Resets the key to the current time, and fails silently when the key isn't
--   present.
touch :: ( Hashable k
         , Eq k
         ) => k -> TimeMap k a -> IO ()
touch = adjust id

{-# INLINEABLE touch #-}

filter :: ( Hashable k
          , Eq k
          ) => (a -> Bool) -> TimeMap k a -> STM ()
filter p = filterWithKey (const p)

{-# INLINEABLE filter #-}

filterWithKey :: ( Hashable k
                 , Eq k
                 ) => (k -> a -> Bool) -> TimeMap k a -> STM ()
filterWithKey p xs = do
  ks <- (HS.toList . MM.elems) <$> readTVar (timeMap xs)
  mapM_ go ks
  where
    go k = HT.focus go' k (keysMap xs)
      where
        go' (Just (TimeIndexed _ x))
          | p k x     = pure ((), F.Keep)
          | otherwise = pure ((), F.Remove)
        go' Nothing   = pure ((), F.Keep)

{-# INLINEABLE filterWithKey #-}

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
  mapM_ (\k -> HT.delete k $ keysMap xs) $! HS.toList toRemove

{-# INLINEABLE filterSince #-}


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
  atomically $ (filterSince $! addUTCTime (negate t) now) xs

{-# INLINEABLE filterFromNow #-}
