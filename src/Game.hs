-- | Game loop module

-- Suggestion: hide explicit w tossing, behind the script, like with GameHandle.
module Game
   ( chessGame
   ) where

import Language
import Game.Control (gameSystemsRunner)
import Game.ResourceManager (fillCaches, flushCaches)

-- | Inits ECS and launch game loop
chessGame :: LangL ()
chessGame = do
   evalResourceManager fillCaches
   -- ^ Fill up all caches this all files
   w <- initWorld
   -- ^ Inits clear world state
   runECSystems w gameSystemsRunner
   -- ^ Runs ECS initialization scripts and game loop
   evalResourceManager flushCaches
   -- ^ Free all hardware memory, and we done.