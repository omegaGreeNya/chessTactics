-- | Module defines initialization and deinitialization
-- of external systems, like window manager, render/audio lib, etc.
{-# LANGUAGE RecordWildCards #-}

module Initialization
   ( initGame
   , shutdownGame
   ) where

import qualified SDL

import Types
   (GameConfig(..), SDLConfig(..), GameHandle(..), SubSystemsHandle(..), SDLHandle(..), AppCaches(..))

import Stores.Cache.Class (new)

initGame :: GameConfig -> IO GameHandle
initGame GameConfig{..} = do
   -- << SubSystems initialization
   SDL.initializeAll
   -- ^ Init SDL
   
   hWindow <- SDL.createWindow cfgWindowName (cfgSDLWindow cfgSDL)
   hRenderer <- SDL.createRenderer hWindow (-1) (cfgSDLRenderer cfgSDL)
   let hSDL = SDLHandle{..}
   -- ^ SDL handle construction
   
   let hSystems = SubSystemsHandle{..}
   -- >> 
   
   -- << Caches initialization
   textureCache <- new
   let hCaches = AppCaches{..}
   -- ^ Init caches.
   -- >>
   
   return GameHandle{..}

shutdownGame :: GameHandle -> IO ()
shutdownGame GameHandle{..} = do
   SDL.destroyRenderer (hRenderer $ hSDL hSystems)
   SDL.destroyWindow (hWindow $ hSDL hSystems)
   SDL.quit