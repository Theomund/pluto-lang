{-# LANGUAGE TemplateHaskell #-}

module Analyzer where

import Control.Lens
import Control.Lens.Extras
import Syntax

makePrisms ''Decl

testFunc :: Decl -> Bool
testFunc = any (is _Var) . universe

checkMain :: [Decl] -> Bool
checkMain xs = False

checkProcedure :: [Decl] -> Bool
checkProcedure [] = False

checkVariable :: [Decl] -> Bool
checkVariable [] = False

checkArgument :: [Decl] -> Bool
checkArgument [] = False

typeCheck :: [Decl] -> Bool
typeCheck [] = False
