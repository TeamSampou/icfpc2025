-- # Lib 雛形モジュール
-- このファイルは`stack new`コマンドで自動的に`src/`に挿入されます
-- 
-- ## 言語拡張と`module`宣言
-- 最低限の指定をしてある
{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Lib
    ( someFunc
    ) where

-- ## `doctest`のための記述と定義本体
{- | 
「なんか関数」を標準出力に印字する
>>> someFunc
なんか関数
-}
someFunc :: IO ()
someFunc = putStrLn "なんか関数"
