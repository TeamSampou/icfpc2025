module Base
  ( Layout
  , RoomLabel
  , RoomIndex
  , Door
  , Plan
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
