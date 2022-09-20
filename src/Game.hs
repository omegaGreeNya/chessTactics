-- | Game loop module

-- Suggestion: hide explicit w tossing, behind the script, like with GameHandle.
module Game
   ( chessGame
   ) where

import Language
import Systems (initSystem, isGameOver)
import Game.ResourceManager (fillCaches, flushCaches)

import Game.Loop (loopStepScript, initLoopState)

-- | Inits ECS and launch game loop
chessGame :: LangL ()
chessGame = do
   evalResourceManager fillCaches
   -- ^ Fill up all caches this all files
   w <- initWorld
   -- ^ Inits clear world state
   applySystem w initSystem
   -- ^ Fills world first state
   runGameLoop w isGameOver loopStepScript initLoopState 
   -- ^ Run game loop
   evalResourceManager flushCaches
   -- ^ Free all hardware memory, and we done.