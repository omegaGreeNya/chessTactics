module Types.Render where

import Foreign.C.Types (CInt)
import Linear.Affine (Point)
import Linear.V2

data Picture = Picture
   { picPosition :: Point V2 CInt
   -- ^ position on screen (0,0) is left upper corner
   , picResource :: Resource
   -- ^ id of resource to render
   } 

data Resource
   = RGeometry Geometry
   -- | RTexture Int
   -- ^ Texture Id
   
data Geometry
   = Square (V2 CInt)
   -- ^ Square defined by 2d vector (X ---> | y /\)