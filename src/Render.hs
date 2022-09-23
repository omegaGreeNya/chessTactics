{-# LANGUAGE RecordWildCards #-}
module Render
   ( flush
   , renderPicture
   , present
   ) where
import Foreign.C.Types (CInt)
import Linear
import Linear.Affine (Point)
import SDL (($=))
import qualified SDL

import Types (Picture (..), RenderResource(..), Geometry(..), Clip, Size, Color)
import Utils (setColor)

renderPicture :: SDL.Renderer -> Picture -> IO ()
renderPicture renderer (Picture pos (RGeometry color geom)) = 
   renderGeometry renderer pos color geom
renderPicture renderer (Picture pos (RTexture (texturePtr, size) clip)) =
   renderTexture renderer texturePtr pos size clip

renderGeometry :: SDL.Renderer -> Point V2 CInt -> Color -> Geometry -> IO ()
renderGeometry renderer pos color (Square size) = do
   setColor renderer color
   SDL.fillRect renderer (Just (SDL.Rectangle pos size))


-- | TO DO
-- add fliping and angling.
renderTexture :: SDL.Renderer  -- handle
              -> SDL.Texture   -- texture
              -> Point V2 CInt -- position
              -> Size          -- texture size (V2 CInt)
              -> Maybe Clip    -- Maybe (Rectangle (Point V2 CInt) (V2 CInt))
              -> IO ()
renderTexture renderer texturePtr pos size clip =
   let dstSize = maybe size (\(SDL.Rectangle _ size') -> size') clip
   in SDL.copyEx renderer
              texturePtr
              clip
              (Just $ SDL.Rectangle pos dstSize)
              0
              Nothing
              (pure False)

flush :: SDL.Renderer -> IO ()
flush renderer = do
   SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
   SDL.clear renderer

present :: SDL.Renderer -> IO ()
present = SDL.present