{-# LANGUAGE OverloadedStrings #-}

module FilesIO where

import Data.Functor
import Data.IORef
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.IO (withFile, IOMode (ReadMode), hGetLine)

import qualified Data.Aeson as J

import TypesJSON (ExploreResponse (..))

timestamp :: IO String
timestamp =
  formatTime defaultTimeLocale "%d-%H%M%S" <$> getZonedTime

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

readExplores :: String -> IO ([[String]], [LB.ByteString])
readExplores name = do
  let plansPath = exploresDir </> name <.> "plans"
      resultsPath = exploresDir </> name <.> "results"
  putStrLn $ "loading " ++ plansPath
  plans <- map read . lines <$> readFile plansPath
  putStrLn $ "loading " ++ resultsPath
  results <- L8.lines <$> LB.readFile resultsPath
  pure (plans, results)

readSolutions :: J.FromJSON a => String -> IO (a, LB.ByteString)
readSolutions name = do
  let guessPath = solutionsDir </> name <.> "guess"
      respPath = solutionsDir </> name <.> "resp"
      decode' bs = maybe (fail $ "readSolutions: decode-error: " ++ show bs) pure $ J.decode bs
  guess <- decode' =<< LB.readFile guessPath
  resp <- LB.readFile respPath
  pure (guess, resp)

getExploreReplay :: IO (String -> String -> IO String, [String] -> IO ([[Int]], Int))
getExploreReplay = do
  nameRef <- newIORef ("unknown", "<no-name>")
  dref <- newIORef ([], [])
  let eselect prob ename = do
        let name = prob </> ename
        (plansList, resListBS) <- readExplores name
        let decode' bs = do
              ExploreResponse rs qc <- maybe (fail $ "decode error: " ++ name) pure $ J.decode bs
              pure (rs, qc)
        resultsList <- mapM decode' resListBS
        writeIORef dref (plansList, resultsList)
        writeIORef nameRef (prob, ename)
        pure prob
  let eexplore plans = do
        (planss, resultss) <- readIORef dref
        let getResults nplanss = case resultss of
              []              -> fail "replay: no left plans"
              rs : nresultss  -> writeIORef dref (nplanss, nresultss) $> rs
        case planss of
          []               -> fail "replay: no left plans"
          ps : nplanss
            | ps /= plans  -> fail $ "inconsistent: " ++ show ps ++ " =/= " ++ show plans
            | otherwise    -> getResults nplanss
  pure (eselect, eexplore)
