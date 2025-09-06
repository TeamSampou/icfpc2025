module ObservationSummary
 ( ObservationSummary
 , Trie (..)
 , singleton
 , union
 , unions
 , fromList
 , toList
 ) where

import Client (RoomLabel, Door)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl1')

import Base


type ObservationSummary = Trie RoomLabel

data Trie a = Node !a (IntMap (Trie a))
  deriving (Show)

singleton :: Plan -> [a] -> Trie a
singleton [] [l] = Node l (IntMap.empty)
singleton (d : plan') (l : ls) = Node l (IntMap.singleton (read [d]) (singleton plan' ls))
singleton _ _ = undefined

union :: (Eq a, Show a) => Trie a -> Trie a -> Trie a
union (Node l1 children1) (Node l2 children2)
  | l1 /= l2 = error ("label mismatch: " ++ show l1 ++ " /= " ++ show l2)
  | otherwise = Node l1 $ IntMap.unionWith union children1 children2

unions :: (Eq a, Show a) => [Trie a] -> Trie a
unions = foldl1' union

fromList :: (Eq a, Show a) => [(Plan, [a])] -> Trie a
fromList = unions . map (uncurry singleton)

toList :: Trie a -> [(Plan, [a])]
toList = f [] []
  where
    f ds ls (Node l children)
      | IntMap.null children = pure (concat $ map show $ reverse ds, reverse (l : ls))
      | otherwise = do
          (d, node) <- IntMap.toList children
          f (d : ds) (l : ls) node
