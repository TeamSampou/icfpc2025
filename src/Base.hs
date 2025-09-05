module Base
  ( Layout
  , RoomLabel
  , RoomIndex
  , Door
  ) where

type Layout = ([RoomLabel], RoomIndex, [((RoomIndex, Door), (RoomIndex, Door))])

-- | 2-bit room labels
type RoomLabel = Int

type RoomIndex = Int

type Door = Int

