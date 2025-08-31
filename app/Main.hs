{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment

main :: IO ()
main = do
    { prog <- getProgName
    ; args <- getArgs
    ; putStrLn $ "program:   " ++ prog
    ; putStrLn $ "arguments: " ++ unwords args
    } 
