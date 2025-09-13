module Client
  ( initClient
  , select
  , explore
  , guess
  , Layout
  , RoomLabel
  , RoomIndex
  , Door
  ) where

import qualified Configuration.Dotenv as DotEnv
import qualified Data.Aeson as J
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Simple as HTTP
import System.Environment (lookupEnv)

import Base
import TypesJSON
import FilesIO (selectStamp, writeExplores, writeSolutions)

-- ------------------------------------------------------------------------

initClient :: IO ()
initClient = DotEnv.loadFile DotEnv.defaultConfig

select :: String -> IO String
select problemName = do
  endPoint <- getEndPoint
  Just teamId <- lookupEnv "ID"
  initReq <- HTTP.parseRequest $ "POST " ++ endPoint ++ "select"
  let req = HTTP.setRequestBodyJSON (Map.fromList [("id", teamId), ("problemName", problemName)]) initReq
  res <- HTTP.httpLbs req
  case J.decode (HTTP.getResponseBody res) of
    Nothing -> fail ("parse error: " ++ show (HTTP.getResponseBody res))
    Just (Success (SelectResponse name)) -> selectStamp name $> name
    Just (Error e) -> fail (errorError e)

explore :: [String] -> IO ([[Int]], Int)
explore plans = do
  endPoint <- getEndPoint
  Just teamId <- lookupEnv "ID"
  initReq <- HTTP.parseRequest $ "POST " ++ endPoint ++ "explore"
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
  endPoint <- getEndPoint
  Just teamId <- lookupEnv "ID"
  initReq <- HTTP.parseRequest $ "POST " ++ endPoint ++ "guess"
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

getEndPoint :: IO String
getEndPoint = fromMaybe "https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com/" <$> lookupEnv "ENDPOINT_URL"
