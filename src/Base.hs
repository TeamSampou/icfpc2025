module Base
  (
  -- * Layout
    Layout
  , RoomLabel
  , RoomIndex
  , Door
  , equivalentLayout

  -- * Plan
  , Plan
  , maxPlan
  , maxPlanFull
  , randomWalk

  -- * Parsed Plan
  , Action (..)
  , ParsedPlan
  , parsePlan
  , renderPlan
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (isJust)
import qualified System.Random.MWC as Rand

type Layout = ([RoomLabel], RoomIndex, [((RoomIndex, Door), (RoomIndex, Door))])

-- | 2-bit room labels
type RoomLabel = Int

type RoomIndex = Int

type Door = Int

type Plan = String

-- | Number of doorways per plan for lightning-round problems
maxPlan :: Int -> Int
maxPlan numRooms = numRooms * 18

-- | Number of doorways per plan for full-round problems
--
-- これはチョークを使う回数を含まない。
maxPlanFull :: Int -> Int
maxPlanFull numRooms = numRooms * 6

randomWalk :: PrimMonad m => Rand.Gen (PrimState m) -> Int -> m Plan
randomWalk gen steps = fmap (concat . map show) $ replicateM steps (Rand.uniformR (0::Door, 5) gen)

-- ------------------------------------------------------------------------

data Action
  = PassDoor !Door
  | AlterLabel !RoomLabel
  deriving (Eq, Ord, Show, Read)

type ParsedPlan = [Action]

parsePlan :: Plan -> ParsedPlan
parsePlan [] = []
parsePlan ('[' : l : ']' : xs) = AlterLabel (read [l]) : parsePlan xs
parsePlan (d : xs) = PassDoor (read [d]) : parsePlan xs

renderPlan :: ParsedPlan -> Plan
renderPlan plan = foldr ($) "" (map f plan)
  where
    f :: Action -> ShowS
    f (PassDoor d) = shows d
    f (AlterLabel l) = showChar '[' . shows l . showChar ']'

-- ------------------------------------------------------------------------

equivalentLayout :: Layout -> Layout -> Bool
equivalentLayout (labels1, staringRoom1, connections1) (labels2, staringRoom2, connections2) = isJust $ do
  guard $ length labels1 == length labels2

  (m1, m2) <- execStateT (f staringRoom1 staringRoom2) (IntMap.empty, IntMap.empty)

  forM_ (zip [0..] labels1) $ \(r1, l1) -> do
    r2 <- IntMap.lookup r1 m1
    let l2 = labels2 !! r2
    guard $ l1 == l2

  forM_ (zip [0..] labels2) $ \(r2, l2) -> do
    r1 <- IntMap.lookup r2 m2
    let l1 = labels1 !! r1
    guard $ l1 == l2

  where
    connections1' :: IntMap (IntMap RoomIndex)
    connections1' = IntMap.unionsWith IntMap.union
      [IntMap.fromList [(r1, IntMap.singleton d1 r2), (r2, IntMap.singleton d2 r1)] | ((r1,d1),(r2,d2)) <- connections1]

    connections2' :: IntMap (IntMap RoomIndex)
    connections2' = IntMap.unionsWith IntMap.union
      [IntMap.fromList [(r1, IntMap.singleton d1 r2), (r2, IntMap.singleton d2 r1)] | ((r1,d1),(r2,d2)) <- connections2]

    f :: RoomIndex -> RoomIndex -> StateT (IntMap RoomIndex, IntMap RoomIndex) Maybe ()
    f currentRoom1 currentRoom2 = do
      (m1, m2) <- get
      if currentRoom1 `IntMap.member` m1 && currentRoom2 `IntMap.member` m2 then
        pure ()
      else do
        m1' <-
          case IntMap.lookup currentRoom1 m1 of
            Nothing -> pure $ IntMap.insert currentRoom1 currentRoom2 m1
            Just r2 -> do
              guard $ r2 == currentRoom2
              pure m1
        m2' <-
          case IntMap.lookup currentRoom2 m2 of
            Nothing -> pure $ IntMap.insert currentRoom2 currentRoom1 m2
            Just r1 -> do
              guard $ r1 == currentRoom1
              pure m2
        put (m1', m2')
        forM_ [0..5] $ \d -> do
          f (connections1' IntMap.! currentRoom1 IntMap.! d) (connections2' IntMap.! currentRoom2 IntMap.! d)

-- ------------------------------------------------------------------------
