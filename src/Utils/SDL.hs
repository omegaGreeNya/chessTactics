-- | Utility functions related to filepath, folders, files, etc.
module Utils.SDL
   ( setColor
   ) where

import SDL (($=))
import qualified SDL

import Types.Picture (Color)

setColor :: SDL.Renderer -> Color -> IO ()
setColor renderer color = SDL.rendererDrawColor renderer $= color
