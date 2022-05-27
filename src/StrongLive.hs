{-# LANGUAGE FlexibleInstances #-}

module StrongLive where

import Data.Bifunctor
import Data.Set

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Maybe as Maybe

import Std (intercalate, join, compose)
import MonotoneFrameworks
import Analyses


instance BoundedSemiLattice (Set String)

moveAlive :: Set String -> String -> Set String -> Set String -> Set String
moveAlive src name freeVars dst = if name `member` src then dst' <> freeVars else dst'
  where
    dst' = delete name dst

keepAlive :: String -> Set String -> Set String -> Set String
keepAlive name freeVars alive = moveAlive alive name freeVars alive

aliveInCall :: [String] -> String -> [Set String] -> Set String -> Set String -> Set String
aliveInCall params returnVar freeVars aliveOutEntry = compose fs . delete returnVar
 where
  move :: String -> Set String -> Set String -> Set String
  move param freeVars alive = if param `member` aliveOutEntry then alive <> freeVars else alive

  fs :: [Set String -> Set String]
  fs = uncurry move <$> zip params freeVars

aliveInExit :: String -> String -> Set String -> Set String
aliveInExit returnVarOut returnVarIn aliveOutReturn = if returnVarOut `member` aliveOutReturn then singleton returnVarIn else mempty
