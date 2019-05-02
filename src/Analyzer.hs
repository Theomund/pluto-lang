{-# LANGUAGE TemplateHaskell #-}

module Analyzer where

import Control.Lens
import Control.Monad (when)
import Data.Data.Lens
import qualified Data.Map.Strict as Map
import Data.Maybe
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

checkFunction :: [Decl] -> IO ()
checkFunction xs = do
  let list = catMaybes $ over mapped (\x -> x ^? _Func . _1) xs
  let names = Seq.fromList list
  let uniqueNames = Set.fromList list
  when (length names /= length uniqueNames) $
    error "A program cannot have duplicate functions."

checkVariable :: [Decl] -> IO ()
checkVariable xs = do
  let list = catMaybes $ over mapped (\x -> x ^? _Var . _1) xs
  let names = Seq.fromList list
  let uniqueNames = Set.fromList list
  when (length names /= length uniqueNames) $
    error "A program cannot have duplicate variables."

checkAssignment :: [Decl] -> IO ()
checkAssignment xs = return ()
