-- | Module exports all systems to modify/render world state.
module Systems 
   ( initSystem
   , stepSystem
   , drawSystem
   ) where

import Apecs (cmap, cmapM_)
import Control.Monad.IO.Class (liftIO)
import Linear.V2

import Render (renderPicture)
import Types
import World (System')
import World.Components

import Systems.Init (initSystem)

stepSystem :: StepTime -> System' ()
stepSystem step = do
   moveEnts step
   debugSystem

drawSystem :: System' ()
drawSystem = do
   renderEnts

moveEnts :: StepTime -> System' ()
moveEnts step =  do
   cmap $ \(CPosition p, CVelocity v) -> CPosition (p + (v * (V2 step step)))

renderEnts :: System' ()
renderEnts = do
   -- clear sreen
   cmapM_ $ \(CRenderable pic) ->
      liftIO $ renderPicture pic

-- <<< Debugging
debugSystem :: System' ()
debugSystem = do
   printPosition

printPosition :: System' ()
printPosition = do
   cmapM_ $ \(CPosition p) ->
      liftIO $ putStrLn (show (CPosition p))
-- >>>