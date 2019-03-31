module Analyzer where

import Syntax

checkMain :: Decl -> IO ()
checkMain (Func "main" [] _) = putStrLn "Found a valid main function."
checkMain _ = return ()
