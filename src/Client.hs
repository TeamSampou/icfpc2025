{-# LANGUAGE DeriveGeneric #-}
module Client where

import qualified Configuration.Dotenv as DotEnv
import Control.Applicative
import qualified Data.Aeson as J
import Data.Char (toLower)
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import System.Environment (lookupEnv)

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
    Just (Success (SelectResponse name)) -> pure name
    Just (Error e) -> fail (errorError e)

explore :: [String] -> IO ([[Int]], Int)
explore plans = do
  Just teamId <- lookupEnv "ID"
  initReq <- HTTP.parseRequest "POST https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/explore"
  let req = HTTP.setRequestBodyJSON (ExploreRequest teamId plans) initReq
  res <- HTTP.httpLbs req
  case J.decode (HTTP.getResponseBody res) of
    Nothing -> fail ("parse error: " ++ show (HTTP.getResponseBody res))
    Just (Success (ExploreResponse results queryCount)) -> pure (results, queryCount)
    Just (Error e) -> fail (errorError e)

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

data SelectResponse
  = SelectResponse
  { selectResponseProblemName :: String
  }
  deriving (Generic)

instance J.ToJSON SelectResponse where
  toEncoding = J.genericToEncoding (customOptions "selectResponse")

instance J.FromJSON SelectResponse where
  parseJSON = J.genericParseJSON (customOptions "selectResponse")

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
