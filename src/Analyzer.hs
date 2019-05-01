{-# LANGUAGE TemplateHaskell #-}

module Analyzer where

import Control.Lens
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Syntax

makePrisms ''Decl

makePrisms ''Expr

makePrisms ''Stmt

makePrisms ''Item

checkMain :: [Decl] -> IO ()
checkMain [] = error "No function declarations were found."
checkMain xs = do
  let names =
        Map.fromList $ over mapped (\x -> (x ^. _Func . _1, x ^. _Func . _2)) xs
  case Map.lookup "main" names of
    Nothing -> error "No main function was found."
    Just [] -> return ()
    Just _ -> error "A main function cannot have any arguments."
