module Main where

import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO

main :: IO ()
main = do
    { pr <- getProgName
    ; as <- getArgs
    ; pj <- takeBaseName <$> getCurrentDirectory
    ; case as of
        ex:_ -> proc pj ex
        _    -> usage pr
    }

proc :: FilePath -> FilePath -> IO ()
proc pj ex = do
    { let
        { normex   = toLower <$> ex
        ; execdir  = "app" </> normex
        ; from     = "app" </> "Main.hs"
        ; to       = execdir </> "Main.hs"
        }
    ; createDirectory execdir
    ; copyFile from to
    ; insEntry pj normex
    }

insEntry :: String -> String -> IO ()
insEntry pj ex = do
    { ls <- lines <$> readFile "package.yaml"
    ; case break ("executables:" `isPrefixOf`) ls of
        (xs,ys) -> do
            { writeFile "tmp.yaml" $ unlines $ xs ++ take 1 ys ++ entries pj ex ++ drop 1 ys
            ; renameFile "tmp.yaml" "package.yaml"
            }
    }

entries :: String -> String -> [String]
entries pj ex = 
    [ "  " ++ ex ++ ":"
    , "    main:               Main.hs"
    , "    source-dirs:        app/" ++ ex
    , "    ghc-options:"
    , "    - -rtsopts"
    , "    dependencies:"
    , "    - " ++ pj
    , ""
    ]

usage :: String -> IO ()
usage pr = hPutStrLn stderr msg
    where
        msg = "Usage: " ++ pr ++ " <entry-name>"
