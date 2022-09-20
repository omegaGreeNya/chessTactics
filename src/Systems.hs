-- | Module exports all systems to modify/render world state.
module Systems 
   ( initSystem
   , stepSystem
   , isGameOver
   , drawSystem
   , userInputSystem
   ) where

import qualified SDL.Event as SDL

import Apecs (cmap, cfold)
import Control.Monad.IO.Class (liftIO)
import Linear.V2

import Types (GameHandle(..))
import Types.Time (HiResTime)
import World (System')
import World.Components

import Systems.Init (initSystem)
import Systems.Render (drawSystem)

stepSystem :: GameHandle -> HiResTime -> System' ()
stepSystem _ dt = do
   let timeToProceed = fromIntegral dt
   moveEnts timeToProceed
   --updateVelocity timeToProceed
   --debugSystem

moveEnts :: Double -> System' ()
moveEnts dt =  do
   cmap $ \(CPosition p, CVelocity v)
      -> CPosition (p + (v * (V2 dt dt)))

{-
updateVelocity :: Double -> System' ()
updateVelocity _ = do
   cmap $ \(CVelocity v)
      -> CVelocity (v + (V2 0.01 0.02))
-}

-- returns true then some entity position by x goes over 1000 of somethings
isGameOver :: System' (Bool)
isGameOver = do
   x <- cfold (\x (CPosition (V2 x' _)) -> max x x') 0
   return $ (abs x) > 1000


-- basicly just clears SDL events queue, 
-- to solve app NOT RESONDING state due filled queue.
userInputSystem :: GameHandle -> HiResTime -> System' ()
userInputSystem _ _ = do
   _ <- liftIO $ SDL.pollEvents
   return ()

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