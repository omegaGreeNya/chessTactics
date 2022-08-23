module Types.Render 
   ( Picture(..)
   , Resource(..)
   , Geometry(..)
   , Color
   ) where

import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear.Affine (Point)
import Linear.V2
import Linear.V4

data Picture = Picture
   { picPosition :: Point V2 CInt
   -- ^ position on screen (0,0) is left upper corner
   , picResource :: Resource
   -- ^ id of resource to render
   } 

data Resource
   = RGeometry Color Geometry
   -- | RTexture Int
   -- ^ Texture Id
   
data Geometry
   = Square (V2 CInt)
   -- ^ Square defined by 2d vector (X ---> | y /\)

-- | Red Green Blue Alpha = color code
type Color = V4 Word8