module Lib
  ( someFunc
  ) where

import Codegen
import Parser
import System.IO
import System.Environment
import Text.Megaparsec

someFunc :: IO ()
someFunc = do
  args <- getArgs
  input <- readFile (head args)
  case parse parser "" input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> generateModule xs
