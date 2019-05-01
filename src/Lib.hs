module Lib
  ( someFunc
  ) where

import           Analyzer
import           Codegen
import           Parser
import           System.Environment
import           System.IO
import           Text.Megaparsec

someFunc :: IO ()
someFunc = do
  args <- getArgs
  input <- readFile $ head args
  case parse parser "" input of
    Left bundle -> putStrLn $ errorBundlePretty bundle
    Right xs -> do
      checkMain xs
      checkFunction xs
      checkVariable xs
      checkAssignment xs
      generateModule xs
