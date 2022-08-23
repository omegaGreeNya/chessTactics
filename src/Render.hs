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

import Types (Picture (..), Resource (..), Geometry(..), Color)

renderPicture :: SDL.Renderer -> Picture -> IO ()
renderPicture renderer (Picture pos (RGeometry color geom)) = 
   renderGeometry renderer pos color geom
--renderPicture _ _ = return ()

renderGeometry :: SDL.Renderer -> Point V2 CInt -> Color -> Geometry -> IO ()
renderGeometry renderer pos color (Square size) = do
   setColor renderer color
   SDL.fillRect renderer (Just (SDL.Rectangle pos size))

flush :: SDL.Renderer -> IO ()
flush renderer = do
   SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
   SDL.clear renderer

present :: SDL.Renderer -> IO ()
present = SDL.present

-- << Utility functions
setColor :: SDL.Renderer -> Color -> IO ()
setColor renderer color = SDL.rendererDrawColor renderer $= color
-- >>
