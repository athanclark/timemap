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
  , -- * Filter
    filterSince
  , filterAgo
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
import Control.Arrow (first)


data TimeMap k a = TimeMap
  { timeMap :: TVar (MM.MultiMap UTCTime k)
  , keysMap :: HT.CuckooHashTable k (UTCTime, TVar a)
  }



newTimeMap :: IO (TimeMap k a)
newTimeMap = TimeMap <$> atomically (newTVar MM.empty)
                     <*> HT.new


-- * Construction

-- | Inserts a key and value into a 'TimeMap' - note that it updates the date
--   __and__ the value of an existing entity.
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


lookup :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> IO (Maybe a)
lookup k xs = do
  mEnt <- HT.lookup (keysMap xs) k
  case mEnt of
    Nothing        -> return Nothing
    Just (_, xVar) -> Just <$> readTVarIO xVar


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


-- * Time-Based Filtering

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
--   > filterAgo 1 -- removes entities older than or equal to one second from now
filterAgo :: ( Hashable k
             , Eq k
             ) => NominalDiffTime -- ^ Assumes a positive distance into the past
               -> TimeMap k a
               -> IO ()
filterAgo t xs = do
  now <- getCurrentTime
  filterSince (addUTCTime (negate t) now) xs
