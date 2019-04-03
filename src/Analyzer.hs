module Analyzer where

import Data.List
import Syntax

checkMain :: [Decl] -> Bool
checkMain [] = False
checkMain (Func "main" _:xs) = True
checkMain (_:xs) = False || checkMain xs

checkProcedure :: [Decl] -> Bool
checkProcedure [] = True
checkProcedure xs = do
  let names = f xs
  length (nub names) == length names
  where
    f [] = []
    f (Func a _:xs) = a : f xs
    f (_:xs) = f xs

checkVariable :: [Decl] -> Bool
checkVariable [] = True
checkVariable xs = do
  let names = f xs
  length (nub names) == length names
  where
    f [] = []
    f (Var a _:xs) = a : f xs
    f (_:xs) = f xs
