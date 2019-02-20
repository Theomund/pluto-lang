module Lib
  ( someFunc
  ) where

import Parser

import Text.Megaparsec

someFunc :: IO ()
someFunc = do
  input <- getContents
  parseTest parser input
