{-# LANGUAGE RecordWildCards #-}

module Types.Game.Config
   ( GameConfig(..)
   , SDLConfig(..)
   , defaultConfig
   ) where

import Data.Text (Text)
import qualified SDL

data GameConfig = GameConfig
   { cfgSDL :: SDLConfig
   , cfgWindowName :: Text
   }

data SDLConfig = SDLConfig
   { cfgSDLWindow   :: SDL.WindowConfig
   , cfgSDLRenderer :: SDL.RendererConfig
   }

defaultConfig :: GameConfig
defaultConfig = let
   cfgSDL = SDLConfig defaultSDLWindow defaultSDLRenderer
   cfgWindowName = "WIP"
   in GameConfig{..}

defaultSDLWindow :: SDL.WindowConfig
defaultSDLWindow = SDL.defaultWindow

defaultSDLRenderer :: SDL.RendererConfig
defaultSDLRenderer = SDL.RendererConfig
   { SDL.rendererType = SDL.AcceleratedRenderer
   , SDL.rendererTargetTexture = False
   }
