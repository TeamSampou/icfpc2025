{-# LANGUAGE DeriveGeneric #-}
module TypesJSON where

import Control.Applicative
import Data.Char (toLower)
import Data.List (stripPrefix)
import GHC.Generics

import qualified Data.Aeson as J

{- API 用の JSON 定義なので、変更するときは API 互換性に注意 -}

-- ------------------------------------------------------------------------

customOptions :: String -> J.Options
customOptions prefix =
  J.defaultOptions
  { J.fieldLabelModifier = \name ->
      case stripPrefix prefix name of
        Nothing -> undefined
        Just name' -> f name'
  }
  where
    f [] = []
    f (c:cs) = toLower c : cs

-- ------------------------------------------------------------------------

data SelectResponse
  = SelectResponse
  { selectResponseProblemName :: String
  }
  deriving (Generic)

instance J.ToJSON SelectResponse where
  toEncoding = J.genericToEncoding (customOptions "selectResponse")

instance J.FromJSON SelectResponse where
  parseJSON = J.genericParseJSON (customOptions "selectResponse")

-- ------------------------------------------------------------------------

data ExploreRequest
  = ExploreRequest
  { exploreRequestId :: String
  , exploreRequestPlans :: [String]
  }
  deriving (Generic)

instance J.ToJSON ExploreRequest where
  toEncoding = J.genericToEncoding (customOptions "exploreRequest")

instance J.FromJSON ExploreRequest where
  parseJSON = J.genericParseJSON (customOptions "exploreRequest")

data ExploreResponse
  = ExploreResponse
  { exploreResponseResults :: [[Int]]
  , exploreResponseQueryCount :: Int
  }
  deriving (Show, Generic)

instance J.ToJSON ExploreResponse where
  toEncoding = J.genericToEncoding (customOptions "exploreResponse")

instance J.FromJSON ExploreResponse where
  parseJSON = J.genericParseJSON (customOptions "exploreResponse")

-- ------------------------------------------------------------------------

data GuessRequest
  = GuessRequest
  { guessRequestId :: String
  , guessRequestMap :: GuessRequestMap
  }
  deriving (Generic)

instance J.ToJSON GuessRequest where
  toEncoding = J.genericToEncoding (customOptions "guessRequest")

instance J.FromJSON GuessRequest where
  parseJSON = J.genericParseJSON (customOptions "guessRequest")

data GuessRequestMap
  = GuessRequestMap
  { guessRequestMapRooms :: [Int]
  , guessRequestMapStartingRoom :: Int
  , guessRequestMapConnections :: [Connection]
  }
  deriving (Show, Generic)

instance J.ToJSON GuessRequestMap where
  toEncoding = J.genericToEncoding (customOptions "guessRequestMap")

instance J.FromJSON GuessRequestMap where
  parseJSON = J.genericParseJSON (customOptions "guessRequestMap")

data Connection
  = Connection
  { connectionFrom :: RoomDoor
  , connectionTo :: RoomDoor
  }
  deriving (Show, Generic)

instance J.ToJSON Connection where
  toEncoding = J.genericToEncoding (customOptions "connection")

instance J.FromJSON Connection where
  parseJSON = J.genericParseJSON (customOptions "connection")

data RoomDoor
  = RoomDoor
  { roomDoorRoom :: Int
  , roomDoorDoor :: Int
  }
  deriving (Show, Generic)

instance J.ToJSON RoomDoor where
  toEncoding = J.genericToEncoding (customOptions "roomDoor")

instance J.FromJSON RoomDoor where
  parseJSON = J.genericParseJSON (customOptions "roomDoor")

data GuessResponse
  = GuessResponse
  { guessResponseCorrect :: Bool
  }
  deriving (Generic)

instance J.ToJSON GuessResponse where
  toEncoding = J.genericToEncoding (customOptions "guessResponse")

instance J.FromJSON GuessResponse where
  parseJSON = J.genericParseJSON (customOptions "guessResponse")

-- ------------------------------------------------------------------------

data OrError a
  = Success a
  | Error ErrorResponse
  deriving (Generic)

instance J.ToJSON a => J.ToJSON (OrError a) where
  toEncoding (Success a) = J.toEncoding a
  toEncoding (Error e) = J.toEncoding e

instance J.FromJSON a => J.FromJSON (OrError a) where
  parseJSON x = (Success <$> J.parseJSON x) <|> (Error <$> J.parseJSON x)

data ErrorResponse
  = ErrorResponse
  { errorError :: String
  }
  deriving (Generic)

instance J.ToJSON ErrorResponse where
  toEncoding = J.genericToEncoding (customOptions "error")

instance J.FromJSON ErrorResponse where
  parseJSON = J.genericParseJSON (customOptions "error")

-- ------------------------------------------------------------------------
