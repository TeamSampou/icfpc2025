module Base
  ( Layout
  , RoomLabel
  , RoomIndex
  , Door
  , Plan
  , maxPlan
  , randomWalk

  -- * Parsed Plan
  , Action (..)
  , ParsedPlan
  , parsePlan
  , renderPlan
  ) where

import Control.Monad
import Control.Monad.Primitive
import qualified System.Random.MWC as Rand

type Layout = ([RoomLabel], RoomIndex, [((RoomIndex, Door), (RoomIndex, Door))])

-- | 2-bit room labels
type RoomLabel = Int

type RoomIndex = Int

type Door = Int

type Plan = String

maxPlan :: Int -> Int
maxPlan numRooms = numRooms * 18

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
renderPlan plan =  foldr ($) "" (map f plan)
  where
    f :: Action -> ShowS
    f (PassDoor d) = shows d
    f (AlterLabel l) = showChar '[' . shows l . showChar ']'

-- ------------------------------------------------------------------------
