-- | Module defines initialization and deinitialization
-- of external systems, like window manager, render/audio lib, etc.
{-# LANGUAGE RecordWildCards #-}

module Initialization
   ( initGame
   , shutdownGame
   ) where

import qualified SDL

import Types (GameConfig(..), SDLConfig(..)
             ,GameHandle(..), SDLHandle(..))

initGame :: GameConfig -> IO GameHandle
initGame GameConfig{..} = do
   SDL.initialize [SDL.InitVideo]
   -- ^ Init SDL
   
   hWindow <- SDL.createWindow cfgWindowName (cfgSDLWindow cfgSDL)
   hRenderer <- SDL.createRenderer hWindow (-1) (cfgSDLRenderer cfgSDL)
   let hSDL = SDLHandle{..}
   -- ^ SDL handle construction
   
   return GameHandle{..}

shutdownGame :: GameHandle -> IO ()
shutdownGame GameHandle{..} = do
   SDL.destroyRenderer (hRenderer hSDL)
   SDL.destroyWindow (hWindow hSDL)
   SDL.quit