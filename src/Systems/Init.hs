module Systems.Init 
   (initSystem) where

import Apecs (newEntity_)
import Linear.V2

import World (System')
import World.Components

initSystem :: System' ()
initSystem = do
   newEntity_ (CPosition (V2 0 0), CVelocity (V2 1 1))