-- This example is basically the same as in Simplest.hs, only it uses
-- GHC's builtin generics instead of explicit instances of ToJSON and
-- FromJSON.

-- We enable the DeriveGeneric language extension so that GHC can
-- automatically derive the Generic class for us.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL

import Library (Library, FromJSON)

eitherEncode :: Either String Library -> BL.ByteString
eitherEncode (Right lib) = encode lib
eitherEncode (Left errorMsg) = BL.pack errorMsg

main :: IO ()
main = do
  contents <- readFile "C:/Users/mhurd/development/workspace/haskell-scratch/json-scratch/src/library.json"
  let decoded = (eitherDecode $ BL.pack contents) :: Either String Library
  putStrLn "Haskell Record"
  putStrLn "=============="
  print decoded
  putStrLn "JSON Doc"
  putStrLn "========"
  BL.putStrLn $ eitherEncode decoded