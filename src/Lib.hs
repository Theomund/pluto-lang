module Lib
  ( someFunc
  ) where

import Codegen
import Parser
import Text.Megaparsec

someFunc :: IO ()
someFunc = do
  input <- getContents
  case parse parser "" input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> generateModule xs
