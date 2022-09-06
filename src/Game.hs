-- | Game loop module
module Game
   ( chessGame
   ) where

import Control.Monad (unless)

import Language
import Systems (initSystem, stepSystem, drawSystem, isGameOver)
import World (World)
import Game.ResourceManager (fillCaches)


-- | Inits ECS and launch game loop
chessGame :: LangL ()
chessGame = do
   evalResourceManager fillCaches
   -- ^ Fill up all caches this all files
   w <- initWorld
   -- ^ Inits clear world state
   applySystem w initSystem
   -- ^ Fills world first state
   gameLoop w

-- | Game loop
gameLoop :: World -> LangL ()
gameLoop w = do
   w `applySystem` (stepSystem (0.1))
   w `applySystem` drawSystem
   gameOver <- w `applySystem` isGameOver
   unless gameOver $
      gameLoop w