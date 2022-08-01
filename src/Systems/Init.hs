module Systems.Init 
   (initSystem) where

import Apecs (newEntity_)
import Linear.Affine(Point(..))
import Linear.V2
import Types (GameHandle)
import Types.Render

import World (System')
import World.Components

initSystem :: GameHandle -> System' ()
initSystem = const initSystem'

initSystem' :: System' ()
initSystem' = do
   newEntity_ 
      (CPosition (V2 0 0), CVelocity (V2 (1) (1))
      , CRenderable 
         (Picture 
            (P (V2 0 0))
            (RGeometry (Square (V2 10 10)))
         )
      )