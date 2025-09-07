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

exploresDir :: FilePath
exploresDir = "explores"

solutionsDir :: FilePath
solutionsDir = "solutions"

selectedFile :: FilePath
selectedFile = exploresDir </> "selected.txt"

selectStamp :: String -> IO ()
selectStamp problemName = do
  createDirectoryIfMissing True exploresDir
  writeFile selectedFile (problemName <> "\n")

getSelected :: IO String
getSelected = do
  exist <- doesFileExist selectedFile
  if exist
    then withFile selectedFile ReadMode hGetLine
    else pure "unknown"

setupExploresDir :: IO (FilePath, String)
setupExploresDir = do
  let dir = setupDir . (exploresDir </>) =<< getSelected
  (,) <$> dir <*> timestamp

setupSolutionsDir :: IO (FilePath, String)
setupSolutionsDir = do
  let dir = setupDir . (solutionsDir </>) =<< getSelected
  (,) <$> dir <*> timestamp

writeExplores :: Show a => a -> LB.ByteString -> IO ()
writeExplores plans results = do
  (dir, name) <- setupExploresDir
  appendFile (dir </> name <.> "plans") (show plans <> "\n")
  LB.appendFile (dir </> name <.> "results") (results <> "\n")

writeSolutions :: J.ToJSON a => a -> LB.ByteString -> IO ()
writeSolutions guessMap resp = do
  (dir, name) <- setupSolutionsDir
  LB.writeFile (dir </> name <.> "guess") (J.encode guessMap <> "\n")
  LB.writeFile (dir </> name <.> "resp") (resp <> "\n")
