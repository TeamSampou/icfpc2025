{-# LANGUAGE DeriveGeneric #-}
module Client
  ( initClient
  , select
  , explore
  , guess
  , Layout
  , RoomLabel
  , RoomIndex
  , Door
  --
  , GuessRequestMap (..)
  ) where

import qualified Configuration.Dotenv as DotEnv
import Control.Applicative
import qualified Data.Aeson as J
import Data.Char (toLower)
import Data.Functor
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import System.Environment (lookupEnv)

import Base
import FilesIO (selectStamp, writeExplores, writeSolutions)

-- ------------------------------------------------------------------------

initClient :: IO ()
initClient = DotEnv.loadFile DotEnv.defaultConfig

select :: String -> IO String
select problemName = do
  Just teamId <- lookupEnv "ID"
  initReq <- HTTP.parseRequest "POST https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/select"
  let req = HTTP.setRequestBodyJSON (Map.fromList [("id", teamId), ("problemName", problemName)]) initReq
  res <- HTTP.httpLbs req
  case J.decode (HTTP.getResponseBody res) of
    Nothing -> fail ("parse error: " ++ show (HTTP.getResponseBody res))
    Just (Success (SelectResponse name)) -> selectStamp name $> name
    Just (Error e) -> fail (errorError e)

explore :: [String] -> IO ([[Int]], Int)
explore plans = do
  Just teamId <- lookupEnv "ID"
  initReq <- HTTP.parseRequest "POST https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/explore"
  let req = HTTP.setRequestBodyJSON (ExploreRequest teamId plans) initReq
  res <- HTTP.httpLbs req
  let respBody = HTTP.getResponseBody res
  writeExplores plans respBody {- writing request and response to files -}
  case J.decode respBody of
    Nothing -> fail ("parse error: " ++ show (HTTP.getResponseBody res))
    Just (Success (ExploreResponse results queryCount)) -> pure (results, queryCount)
    Just (Error e) -> fail (errorError e)

guess :: Layout -> IO Bool
guess (rooms, startingRoom, connections) = do
  Just teamId <- lookupEnv "ID"
  initReq <- HTTP.parseRequest "POST https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/guess"
  let guessMap = GuessRequestMap rooms startingRoom [Connection (RoomDoor room1 door1) (RoomDoor room2 door2) | ((room1,door1), (room2,door2)) <- connections]
  let req = HTTP.setRequestBodyJSON (GuessRequest teamId guessMap) initReq
  res <- HTTP.httpLbs req
  let respBody = HTTP.getResponseBody res
  writeSolutions guessMap respBody
  case J.decode (HTTP.getResponseBody res) of
    Nothing -> fail ("parse error: " ++ show (HTTP.getResponseBody res))
    Just (Success (GuessResponse correct)) -> pure correct
    Just (Error e) -> fail (errorError e)

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
  deriving (Generic)

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
