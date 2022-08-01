module Types.Game.Handle
   ( GameHandle (..)
   , SDLHandle (..)
   ) where

import qualified SDL

data GameHandle = GameHandle
   { hSDL :: SDLHandle 
   }
   
data SDLHandle = SDLHandle
   { hWindow   :: SDL.Window
   , hRenderer :: SDL.Renderer
   }