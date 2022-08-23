-- | Module exports all systems to modify/render world state.
module Systems 
   ( initSystem
   , stepSystem
   , isGameOver
   , drawSystem
   ) where

import Apecs (cmap, cfold)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Linear.V2

import Types (GameHandle(..), StepTime)
import World (System')
import World.Components

import Systems.Init (initSystem)
import Systems.Render (drawSystem)

stepSystem :: StepTime -> GameHandle -> System' ()
stepSystem dt = const (stepSystem' dt)

stepSystem' :: StepTime -> System' ()
stepSystem' step = do
   moveEnts step
   --debugSystem

moveEnts :: StepTime -> System' ()
moveEnts step =  do
   cmap $ \(CPosition p, CVelocity v) -> CPosition (p + (v * (V2 step step)))

isGameOver :: GameHandle -> System' (Bool)
isGameOver = const isGameOver'

-- returns true then some entity position by x goes over 100 somethings
isGameOver' :: System' (Bool)
isGameOver' = do
   liftIO $ threadDelay 1000
   x <- cfold (\x (CPosition (V2 x' _)) -> max x x') 0
   return $ (abs x) > 100

-- <<< Debugging
{-
debugSystem :: System' ()
debugSystem = do
   printPosition

printPosition :: System' ()
printPosition = do
   cmapM_ $ \(CPosition p) ->
      liftIO $ putStrLn (show (CPosition p))
-}
-- >>>