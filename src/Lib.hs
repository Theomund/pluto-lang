module Lib
  ( compile
  ) where

import Analyzer
import Codegen
import Parser
import System.Environment
import System.FilePath
import System.IO
import Text.Megaparsec

-- | The 'compile' function begins the compilation process.
compile :: IO ()
compile = do
  args <- getArgs
  input <- readFile $ head args
  let name = replaceExtension (head args) ".o"
  case parse parser "" input of
    Left bundle -> putStrLn $ errorBundlePretty bundle
    Right xs -> do
      checkMain xs
      checkFunction xs
      checkVariable xs
      generateModule xs name
