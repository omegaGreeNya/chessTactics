module Types.Picture
   ( Picture(..)
   -- ^ GOD-Record = Position + GOD-ADT.
   , RenderResource(..)
   -- ^ GOD-ADT for all render types.
   , Geometry(..)
   -- ^ Primitives ADT.
   , TextureRaw
   -- ^ Pair of Texture thardware id and it's size.
   , Clip
   , Size
   -- ^ Texture clip, size.
   , Color
   -- ^ Color code.
   ) where

import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Linear.Affine (Point)
import Linear.V2
import Linear.V4

import qualified SDL (Texture, Rectangle)

-- | GOD-Record = Position + GOD-ADT
data Picture = Picture
   { picPosition :: Point V2 CInt
   -- ^ position on screen (0,0) is left upper corner.
   , picResource :: RenderResource
   -- ^ Resource to render, or it's id.
   } 

-- | GOD-ADT for all render types
data RenderResource
   = RGeometry Color Geometry
   | RTexture TextureRaw (Maybe Clip)

-- | Part of the texture that would be cutted from original
-- data Rectangle a = Rectangle (Point V2 a) (V2 a)
type Clip = SDL.Rectangle CInt
   
-- | Primitives ADT
data Geometry
   = Square (V2 CInt)
   -- ^ Square defined by 2d vector (X --->, y /\)

-- | Texture hardware id (SDL part) and it's size.
type TextureRaw = (SDL.Texture, Size)

type Size = V2 CInt

-- | Red Green Blue Alpha = color code
type Color = V4 Word8