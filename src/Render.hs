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

import Types (Picture (..), Resource (..), Geometry(..))

renderPicture :: SDL.Renderer -> Picture -> IO ()
renderPicture renderer (Picture pos (RGeometry geom)) = 
   renderGeometry renderer pos geom
--renderPicture _ _ = return ()

renderGeometry :: SDL.Renderer -> Point V2 CInt -> Geometry -> IO ()
renderGeometry renderer pos (Square size) = do
   SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
   SDL.fillRect renderer (Just (SDL.Rectangle pos size))

flush :: SDL.Renderer -> IO ()
flush renderer = do
   SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
   SDL.clear renderer

present :: SDL.Renderer -> IO ()
present = SDL.present