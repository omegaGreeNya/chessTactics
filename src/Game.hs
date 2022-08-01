-- | Game loop module
module Game
   ( run
   ) where

import Apecs (runSystem)

-- << ECS
import Systems (initSystem, stepSystem, drawSystem)
import World (World, initWorld)


-- | Creates world with intities and runs systems-loops to modify/render world
run :: a -> IO ()
run _ = do
   w <- initWorld
   -- ^ Creates world record with all storage references
   runSystem initSystem w
   -- fork phisic thread here
   loopPhysics w
   -- fork logic thread here
   -- loopLogic w
   -- fork render thread here
   -- loopRender

loopPhysics :: World -> IO ()
loopPhysics w = do
   runSystem (stepSystem (0.1)) w
   -- step phisic by decimal of second
   loopPhysics w

loopRender :: World -> IO ()
loopRender w = do
   runSystem drawSystem w
   --SDL.delay ?fps?
   loopRender w
