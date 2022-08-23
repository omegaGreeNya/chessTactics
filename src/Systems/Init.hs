module Systems.Init 
   (initSystem) where

import Apecs (newEntity_)
import Linear.Affine(Point(..))
import Linear.V2
import Linear.V4
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
            (RGeometry 
               (V4 maxBound 0 0 maxBound)
               (Square (V2 10 10))
            )
         )
      )
   newEntity_
      (CRenderable (Picture (P (V2 0 0)) (RGeometry ((V4 0 0 maxBound maxBound))(Square (V2 100 1000000)))))