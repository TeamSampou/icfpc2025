{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (try, IOException)
import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Network.Wai.Handler.Warp
import Options.Applicative hiding (Success)
import Servant.API
import Servant.Server hiding (layout)

import Base
import qualified FilesIO
import qualified Simulate
import TypesJSON

-- ------------------------------------------------------------------------

type SelectAPI = "select" :> ReqBody '[JSON] TypesJSON.SelectRequest :> Post '[JSON] (TypesJSON.OrError TypesJSON.SelectResponse)

type ExploreAPI = "explore" :> ReqBody '[JSON] TypesJSON.ExploreRequest :> Post '[JSON] (TypesJSON.OrError TypesJSON.ExploreResponse)

type GuessAPI = "guess" :> ReqBody '[JSON] TypesJSON.GuessRequest :> Post '[JSON] (TypesJSON.OrError TypesJSON.GuessResponse)

type API = SelectAPI
      :<|> ExploreAPI
      :<|> GuessAPI

api :: Proxy API
api = Proxy

server :: Map String (FilePath, Bool) -> IORef (Maybe (Layout, Bool, Int)) -> Server API
server problems ref
       = select
    :<|> explore
    :<|> guess
  where
    select :: SelectRequest -> Handler (OrError SelectResponse)
    select req = do
      let name = selectRequestProblemName req
      case Map.lookup name problems of
        Nothing -> pure $ Error $ ErrorResponse $ "unknown problem: " ++ show name
        Just (fname, isFullRound) -> do
          liftIO $ do
            r <- try (FilesIO.readLayoutFromGuessFile fname)
            case r of
              Left (err :: IOException) -> do
                pure $ Error $ ErrorResponse $ show err
              Right layout -> do
                writeIORef ref (Just (layout, isFullRound, 0))
                pure $ Success $ SelectResponse name

    explore :: ExploreRequest -> Handler (OrError ExploreResponse)
    explore req = do
      let plans = exploreRequestPlans req
      state <- liftIO $ readIORef ref
      case state of
        Nothing -> pure $ Error $ ErrorResponse "problem not selected"
        Just (layout@(labels, _, _), isFullRound, queryCount) -> do
          let numRooms = length labels
              maxDoorOpen = if isFullRound then maxPlan numRooms else maxPlanFull numRooms
          if or [length [() | PassDoor _ <- parsePlan plan] > maxDoorOpen | plan <- plans] then do
            pure $ Error $ ErrorResponse "Error: Route plan is too long."
{-
          else if not isFullRound && or [True | plan <- plans, AlterLabel _ <- parsePlan plan] then
            pure $ Error $ ErrorResponse "Error: Cannot alter labels in lightning-round problem"
-}
          else do
            let results = Simulate.explore layout plans
                queryCount' = queryCount + length plans + 1
            liftIO $ writeIORef ref (Just (layout, isFullRound, queryCount'))
            pure $ Success $ ExploreResponse results queryCount'

    guess :: GuessRequest -> Handler (OrError GuessResponse)
    guess req = do
      state <- liftIO $ readIORef ref
      let guessedLayout =
            case guessRequestMap req of
              GuessRequestMap roomLabels startingRooms connects ->
                (roomLabels, startingRooms, [ ((a,b),(c,d)) | (Connection (RoomDoor a b) (RoomDoor c d)) <- connects ])
      case state of
        Nothing -> pure $ Error $ ErrorResponse "problem not selected"
        Just (layout, _isFullRound, _queryCount) -> do
          liftIO $ writeIORef ref Nothing
          pure $ Success $ GuessResponse (equivalentLayout guessedLayout layout)

-- ------------------------------------------------------------------------

data Options
  = Options
  { optPort :: Int
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> port
  where
    port = option auto
      $  short 'p'
      <> long "port"
      <> metavar "N"
      <> value 8080
      <> help "Port number"

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> helper)
  $  fullDesc
  <> header "server"

main :: IO ()
main = do
  opt <- execParser parserInfo

  let problems = Map.fromList
        [ ("secundus", ("solutions/secundus/07-2041.guess", False))
        , ("tertius",  ("solutions/tertius/07-205249.guess", False))
        , ("aleph",    ("solutions/aleph/08-105823.guess", True))
        , ("beth",     ("solutions/beth/08-162028.guess", True))
        , ("gimel",    ("solutions/gimel/08-191010.guess", True))
        , ("vau",      ("solutions/vau/08-200628.guess", True))
        ]
  ref <- newIORef Nothing
  let app = serve api (server problems ref)
  run (optPort opt) app

-- ------------------------------------------------------------------------
