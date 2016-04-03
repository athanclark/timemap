module Data.TimeMap.Internal where

import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import qualified Data.HashSet    as HS


type MultiMap k a = Map.Map k (HS.HashSet a)


empty :: MultiMap k a
empty = Map.empty

insert :: ( Ord k
          , Hashable a
          , Eq a
          ) => k -> a -> MultiMap k a -> MultiMap k a
insert k x = Map.insertWith (HS.union) k (HS.singleton x)

{-# INLINEABLE insert #-}

lookup :: ( Ord k
          ) => k -> MultiMap k a -> HS.HashSet a
lookup k xs =
  case Map.lookup k xs of
    Nothing -> HS.empty
    Just ys -> ys

{-# INLINEABLE lookup #-}

-- | Deletes all elements at @k@
delete :: ( Ord k
          ) => k -> MultiMap k a -> MultiMap k a
delete = Map.delete

{-# INLINEABLE delete #-}

-- | Deletes only the element @a@ from the referenced key @k@
remove :: ( Ord k
          , Hashable a
          , Eq a
          ) => k -> a -> MultiMap k a -> MultiMap k a
remove k x = Map.update go k
  where
    go s = let s' = HS.delete x s
           in if s' == HS.empty
              then Nothing
              else Just s'

{-# INLINEABLE remove #-}

elems :: ( Hashable a
         , Eq a
         ) => MultiMap k a -> HS.HashSet a
elems = foldr HS.union HS.empty

{-# INLINEABLE elems #-}

toList :: ( Hashable a
          , Eq a
          ) => MultiMap k a -> [(k, a)]
toList =
  Map.foldrWithKey go []
  where
    go :: k -> HS.HashSet a -> [(k, a)] -> [(k, a)]
    go k x acc = repeat k `zip` HS.toList x ++ acc
