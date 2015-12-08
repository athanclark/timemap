module Data.TimeMap
  ( TimeMap
  , empty
  , insert
  , lookup
  , delete
  , after
  , ago
  ) where

import Prelude hiding (lookup)
import Data.Time (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Hashable (Hashable (..))
import qualified Data.Map              as Map
import qualified Data.HashMap.Lazy     as HM
import qualified Data.HashSet          as HS
import qualified Data.TimeMap.Internal as MM
import Control.Concurrent.STM
import Control.Arrow (first)


newtype TimedContent k a = TimedContent
  { getTimedContent :: (k, TVar a)
  }

instance Hashable k => Hashable (TimedContent k a) where
  hashWithSalt i (TimedContent (k,_)) = hashWithSalt i k
  hash (TimedContent (k,_))           = hash k

instance Eq k => Eq (TimedContent k a) where
  (TimedContent (x,_)) == (TimedContent (y,_)) = x == y

data TimeMap k a = TimeMap
  { timeMap :: TVar (MM.MultiMap UTCTime (TimedContent k a))
  , keysMap :: TVar (HM.HashMap  k       (UTCTime, TVar a))
  }



empty :: STM (TimeMap k a)
empty = TimeMap <$> newTVar MM.empty <*> newTVar HM.empty

-- | Inserts a key and value into a 'TimeMap' - note that it updates the date
--   __and__ the value of an existing entity.
insert :: ( Hashable k
          , Eq k
          ) => k -> a -> TimeMap k a -> IO (TimeMap k a)
insert k x xs = do
  ks <- readTVarIO (keysMap xs)
  case HM.lookup k ks of
    Nothing -> do
      now <- getCurrentTime
      atomically $ do
        xVar <- newTVar x
        modifyTVar (timeMap xs) $
          MM.insert now $ TimedContent (k, xVar)
        modifyTVar (keysMap xs) $
          HM.insert k (now, xVar)
      return xs
    Just (oldTime, xVar) -> do
      let content = TimedContent (k, xVar)
      now <- getCurrentTime
      atomically $ do
        modifyTVar (timeMap xs)
          (MM.insert now content . MM.remove oldTime content)
        modifyTVar (keysMap xs) $
          HM.adjust (first $ const now) k
        writeTVar xVar x
      return xs


lookup :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM (Maybe a)
lookup k xs = do
  ks <- readTVar (keysMap xs)
  case HM.lookup k ks of
    Nothing        -> return Nothing
    Just (_, xVar) -> Just <$> readTVar xVar


-- | Adjusts the value at @k@, while updating its time.
adjust :: ( Hashable k
          , Eq k
          ) => (a -> a) -> k -> TimeMap k a -> IO (TimeMap k a)
adjust f k xs = do
  ks <- readTVarIO (keysMap xs)
  case HM.lookup k ks of
    Nothing              -> return xs
    Just (oldTime, xVar) -> do
      let content = TimedContent (k, xVar)
      now <- getCurrentTime
      atomically $ do
        modifyTVar (timeMap xs)
          (MM.insert now content . MM.remove oldTime content)
        modifyTVar (keysMap xs) $
          HM.adjust (first $ const now) k
        modifyTVar xVar f
      return xs


delete :: ( Hashable k
          , Eq k
          ) => k -> TimeMap k a -> STM (TimeMap k a)
delete k xs = do
  ks <- readTVar (keysMap xs)
  case HM.lookup k ks of
    Nothing              -> return xs
    Just (oldTime, xVar) -> do
      modifyTVar (timeMap xs)
        (MM.remove oldTime $ TimedContent (k, xVar))
      modifyTVar (keysMap xs)
        (HM.delete k)
      return xs


-- | Filters out all entries after a designated time
after :: ( Hashable k
         , Eq k
         ) => UTCTime
           -> TimeMap k a
           -> STM (TimeMap k a)
after t xs = do
  ts <- readTVar (timeMap xs)
  let (toCut, result) = Map.split t ts
      toCut' = HS.map (fst . getTimedContent) $ MM.elems toCut
  writeTVar (timeMap xs) result
  modifyTVar (keysMap xs) $
    HM.filterWithKey (\k _ -> k `HS.member` toCut')
  return xs


-- | Filters out all entries to be within some time frame
--
--   > ago 1000000 -- removes entities older than one second from now
ago :: ( Hashable k
       , Eq k
       ) => NominalDiffTime -- ^ Assumes a positive distance into the past
         -> TimeMap k a
         -> IO (TimeMap k a)
ago t xs = do
  now <- getCurrentTime
  atomically $ after (addUTCTime (negate t) now) xs
