module Simulate
  ( explore
  , explore1
  ) where

import Control.Monad.ST
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Base


explore :: Layout -> [Plan] -> [[RoomLabel]]
explore layout plans = map (explore1 layout) plans

explore1 :: Layout -> Plan -> [RoomLabel]
explore1 (labels, startingRoom, connections) plan = go (Seq.fromList labels) startingRoom (parsePlan plan)
  where
    go :: Seq RoomLabel -> RoomIndex -> ParsedPlan -> [RoomLabel]
    go currentLabels currentRoom xs =
      Seq.index currentLabels currentRoom :
        case xs of
          [] -> []
          (PassDoor d : xs') -> go currentLabels (connectionTable IntMap.! currentRoom IntMap.! d) xs'
          (AlterLabel l : xs') -> go (Seq.update currentRoom l currentLabels) currentRoom xs'

    connectionTable :: IntMap (IntMap RoomIndex)
    connectionTable = IntMap.unionsWith IntMap.union
      [ IntMap.fromListWith IntMap.union [(r1, IntMap.singleton d1 r2), (r2, IntMap.singleton d2 r1)]
      | ((r1,d1), (r2,d2)) <- connections
      ]

_test :: Bool
_test = explore layout plans == results
   where
     layout =
       ( [0,1,2]
       , 0
       , [ ((0,0),(1,2))
         , ((0,1),(0,1))
         , ((0,2),(1,5))
         , ((0,3),(2,0))
         , ((0,4),(2,2))
         , ((0,5),((2,3)))
         , ((1,0),(2,1))
         , ((1,1),(1,4))
         , ((1,3), (2,5))
         , ((2,4),(2,4))
         , ((2,5),(1,3))
         ]
       )
     plans = ["000","123","213","333"] ++ ["4","5","02","03","04","05","31","32","34","35"]
     results = [[0,1,2,0],[0,0,1,2],[0,1,1,2],[0,2,0,2]] ++ [[0,2],[0,2],[0,1,0],[0,1,2],[0,1,1],[0,1,0],[0,2,1],[0,2,0],[0,2,2],[0,2,1]]
