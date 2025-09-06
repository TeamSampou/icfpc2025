module ObservationSummary
 ( ObservationSummary (..)
 , singleton
 , union
 , unions
 , fromList
 ) where

import Client (RoomLabel, Door)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl1')
import Text.Printf

import Base


data ObservationSummary = Node !RoomLabel (IntMap ObservationSummary)
  deriving (Show)

singleton :: Plan -> [RoomLabel] -> ObservationSummary
singleton [] [l] = Node l (IntMap.empty)
singleton (d : plan') (l : ls) = Node l (IntMap.singleton (read [d]) (singleton plan' ls))
singleton _ _ = undefined

union :: ObservationSummary -> ObservationSummary -> ObservationSummary
union (Node l1 children1) (Node l2 children2)
  | l1 /= l2 = error (printf "label mismatch: %d /= %d" l1 l2)
  | otherwise = Node l1 $ IntMap.unionWith union children1 children2

unions :: [ObservationSummary] -> ObservationSummary
unions = foldl1' union

fromList :: [(Plan, [RoomLabel])] -> ObservationSummary
fromList = unions . map (uncurry singleton)
