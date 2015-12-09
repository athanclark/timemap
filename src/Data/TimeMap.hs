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
  , keys
  , elems
  , -- * Filter
    filterSince
  , filterFromNow
  ) where

import Prelude hiding (lookup, null)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Hashable (Hashable (..))
import Data.Maybe (fromMaybe)
import qualified Data.Map                 as Map
import qualified Data.HashTable.IO        as HT
import qualified Data.HashSet             as HS
import qualified Data.TimeMap.Internal    as MM
import Control.Concurrent.STM


-- | A mutable reference for a time-indexed map, similar to a 'Data.STRef.STRef'.
data TimeMap k a = TimeMap
  { timeMap :: TVar (MM.MultiMap UTCTime k)
  , keysMap :: HT.CuckooHashTable k (UTCTime, TVar a)
  }




-- | Create a fresh, empty map.
newTimeMap :: IO (TimeMap k a)
newTimeMap = TimeMap <$> atomically (newTVar MM.empty)
                     <*> HT.new

-- | Inserts a key and value into a 'TimeMap' - it adds the value
--   or overwites an existing entity.
insert :: ( Hashable k
          , Eq k
          ) => k -> a -> TimeMap k a -> IO ()
insert k x xs = do
  mEnt <- HT.lookup (keysMap xs) k
  now <- getCurrentTime
  xVar <- atomically $ do
    case mEnt of
      Nothing -> do
        xVar <- newTVar x
        modifyTVar (timeMap xs) $ MM.insert now k
        return xVar
      Just (oldTime, xVar) -> do
        modifyTVar (timeMap xs)
          (MM.insert now k . MM.remove oldTime k)
        writeTVar xVar x
        return xVar
  HT.insert (keysMap xs) k (now, xVar)

-- | Performs a non-mutating lookup for some key.
lookup :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> IO (Maybe a)
lookup k xs = do
  mEnt <- HT.lookup (keysMap xs) k
  case mEnt of
    Nothing        -> return Nothing
    Just (_, xVar) -> Just <$> readTVarIO xVar

keys :: ( Hashable k
        , Eq k
        ) => TimeMap k a -> IO (HS.HashSet k)
keys xs = MM.elems <$> readTVarIO (timeMap xs)

elems :: TimeMap k a -> IO [a]
elems xs = do
  refs <- HT.foldM (\acc (_,(_,v)) -> return $ acc ++ [v]) [] (keysMap xs)
  atomically $ mapM readTVar refs


-- | Adjusts the value at @k@, while updating its time.
adjust :: ( Hashable k
          , Eq k
          ) => (a -> a) -> k -> TimeMap k a -> IO ()
adjust f k xs = do
  mEnt <- HT.lookup (keysMap xs) k
  case mEnt of
    Nothing              -> return ()
    Just (oldTime, xVar) -> do
      now <- getCurrentTime
      atomically $ do
        modifyTVar (timeMap xs)
          (MM.insert now k . MM.remove oldTime k)
        modifyTVar xVar f
      HT.insert (keysMap xs) k (now, xVar)


-- | Deletes the value at @k@.
delete :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> IO ()
delete k xs = do
  mEnt <- HT.lookup (keysMap xs) k
  case mEnt of
    Nothing          -> return ()
    Just (oldTime,_) -> do
      atomically $ modifyTVar' (timeMap xs) $ MM.remove oldTime k
      HT.delete (keysMap xs) k



-- | Filters out all entries older than or equal to a designated time
filterSince :: ( Hashable k
               , Eq k
               ) => UTCTime
                 -> TimeMap k a
                 -> IO ()
filterSince t xs = do
  toRemove <- atomically $ do
    ts <- readTVar (timeMap xs)
    let (toCut, mx, result) = Map.splitLookup t ts
        found    = fromMaybe HS.empty mx
        toRemove = MM.elems toCut `HS.union` found
    writeTVar (timeMap xs) result
    return toRemove
  mapM_ (HT.delete $ keysMap xs) (HS.toList toRemove)


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
  filterSince (addUTCTime (negate t) now) xs
