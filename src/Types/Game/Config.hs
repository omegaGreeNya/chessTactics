{-# LANGUAGE RecordWildCards #-}

-- TO DO
-- AppCacheConfig
module Types.Game.Config
   ( GameConfig(..)
   , SDLConfig(..)
   , defaultConfig
   ) where

import Data.Text (Text)
import qualified SDL

data GameConfig = GameConfig
   { cfgSDL :: SDLConfig
   --, cfgCaches :: AppCacheConfig
   , cfgWindowName :: Text
   , cfgUseMetrics :: Bool
   -- ^ Then true, programm will collect and process 
   -- ECS systems execution meta-data. Like Executions per sec.
   }

data SDLConfig = SDLConfig
   { cfgSDLWindow   :: SDL.WindowConfig
   , cfgSDLRenderer :: SDL.RendererConfig
   }

defaultConfig :: GameConfig
defaultConfig = let
   cfgSDL = SDLConfig defaultSDLWindow defaultSDLRenderer
   cfgWindowName = "WIP"
   cfgUseMetrics = True
   in GameConfig{..}

defaultSDLWindow :: SDL.WindowConfig
defaultSDLWindow = SDL.defaultWindow

defaultSDLRenderer :: SDL.RendererConfig
defaultSDLRenderer = SDL.RendererConfig
   { SDL.rendererType = SDL.AcceleratedRenderer
   , SDL.rendererTargetTexture = False
   }
