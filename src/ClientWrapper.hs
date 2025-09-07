module ClientWrapper where

-- import Base
import Client qualified as CLI
import FilesIO qualified as FIO

data Driver =
  Driver
  { rname        :: String
  , rinitClient  :: IO ()
  , rselect      :: String -> String -> IO String
  , rexplore     :: [String] -> IO ([[Int]], Int)
  }

getClient :: IO Driver
getClient =
  pure
  Driver
  { rname        = "<real client>"
  , rinitClient  = CLI.initClient
  , rselect      = \prob _ -> CLI.select prob
  , rexplore     = CLI.explore
  }

getReplay :: IO Driver
getReplay = do
  (selectRep, exploreRep) <- FIO.getExploreReplay
  pure
    Driver
    { rname       = "<replay>"
    , rinitClient  = pure ()
    , rselect      = selectRep
    , rexplore     = exploreRep
    }
