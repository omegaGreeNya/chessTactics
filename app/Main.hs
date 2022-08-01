-- TO DO
-- Add stm and fork ECS-system threads
module Main where

import Game (chessGame)

import Interpreters (launchGame)
import Language

import Types.Game.Config (defaultConfig)

main :: IO ()
main = launchGame $ do
   gameHandle <- initGame defaultConfig
   run gameHandle (chessGame gameHandle)
   -- ^ i know it's ugly, i will rework
   shutDown gameHandle