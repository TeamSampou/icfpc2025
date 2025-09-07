{-# LANGUAGE OverloadedStrings #-}

module FilesIO where

import Data.ByteString.Lazy qualified as LB
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.FilePath ((</>), (<.>))
import System.Directory
import System.IO (withFile, IOMode (ReadMode), hGetLine)

import qualified Data.Aeson as J

timestamp :: IO String
timestamp =
  formatTime defaultTimeLocale "%d-%H%M" <$> getZonedTime

setupDir :: FilePath -> IO FilePath
setupDir dir = do
  createDirectoryIfMissing True dir
  pure dir

-----

data State =
  State
  { problem    :: String
  , tsname     :: String
  } deriving (Show, Read)

exploresDir :: FilePath
exploresDir = "explores"

solutionsDir :: FilePath
solutionsDir = "solutions"

selectedFile :: FilePath
selectedFile = exploresDir </> "selected.txt"

selectStamp :: String -> IO ()
selectStamp problemName = do
  createDirectoryIfMissing True exploresDir
  ts <- timestamp
  let s = State{problem = problemName, tsname = ts}
  writeFile selectedFile (show s <> "\n")

getSelected :: IO State
getSelected = do
  exist <- doesFileExist selectedFile
  if exist
    then readIO =<< withFile selectedFile ReadMode hGetLine
    else fallback
  where fallback = do
          ts <- timestamp
          pure $ State{problem = "unknown", tsname = ts}

setupExploresDir :: IO (FilePath, State)
setupExploresDir = do
  s <- getSelected
  let dir = setupDir (exploresDir </> problem s)
  (,) <$> dir <*> pure s

setupSolutionsDir :: IO (FilePath, State)
setupSolutionsDir = do
  s <- getSelected
  let dir = setupDir (solutionsDir </> problem s)
  (,) <$> dir <*> pure s

writeExplores :: Show a => a -> LB.ByteString -> IO ()
writeExplores plans results = do
  (dir, s) <- setupExploresDir
  let name = tsname s
  appendFile (dir </> name <.> "plans") (show plans <> "\n")
  LB.appendFile (dir </> name <.> "results") (results <> "\n")

writeSolutions :: J.ToJSON a => a -> LB.ByteString -> IO ()
writeSolutions guessMap resp = do
  (dir, s) <- setupSolutionsDir
  let name = tsname s
  LB.writeFile (dir </> name <.> "guess") (J.encode guessMap <> "\n")
  LB.writeFile (dir </> name <.> "resp") (resp <> "\n")
