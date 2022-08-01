-- | Game loop module
module Game
   ( chessGame
   ) where

import Control.Monad (unless)

import Language
import Systems (initSystem, stepSystem, drawSystem, isGameOver)
import Types (GameHandle)
import World (World)


-- | Inits ECS and launch game loop
chessGame :: GameHandle -> LangL ()
chessGame h = do
   w <- initWorld
   applySystem w initSystem
   gameLoop h w

-- | Game loop
gameLoop :: GameHandle -> World -> LangL ()
gameLoop h w = do
   w `applySystem` (stepSystem (0.1))
   w `applySystem` drawSystem
   gameOver <- w `applySystem` isGameOver
   unless gameOver $
      gameLoop h w