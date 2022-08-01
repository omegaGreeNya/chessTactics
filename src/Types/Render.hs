module Types.Render where

import Linear.V2

data Picture = Picture
   { picPosition :: V2 Int
   -- ^ position on screen (0,0) is left upper corner
   , picResource :: Resource
   -- ^ id of resource to render
   } 

data Resource
   = RGeometry Geometry
   -- | RTexture Int
   -- ^ Texture Id
   
data Geometry
   = Square (V2 Int)
   -- ^ Square defined by 2d vector (X ---> | y /\)