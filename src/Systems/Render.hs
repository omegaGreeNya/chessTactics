{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Systems.Render
   (drawSystem) where

import Apecs (Not(..), cmapM_, cmapM)
import Control.Monad.IO.Class (liftIO)
import Foreign.C.Types (CInt)
import Linear.V2

import Linear.Affine (Point(..))

-- << ECS
import World (System')
import World.Components
-- >>

import Render (flush, renderPicture, present)
import StackMetrics (drawMetrics)
import Types ( GameHandle(..), SubSystemsHandle(..), SDLHandle(..)
             , Picture(..))

import qualified SDL

drawSystem :: GameHandle -> Double -> System' ()
drawSystem h@GameHandle{..} interpTime = do
   let renderer = hRenderer . hSDL $ hSystems
   liftIO $ flush renderer
   -- ^ clean up
   drawSystem' renderer interpTime
   -- ^ draw game
   liftIO $ drawMetrics h
   -- ^ draw metrics if possible
   liftIO $ present renderer
   -- ^ Swap buffers

drawSystem' :: SDL.Renderer -> Double -> System' ()
drawSystem' h interpTime = do
   renderBackground h
   -- ^ draw backghround
   renderEnts h interpTime
   -- ^ draw actors
   --renderUI
   -- ^ draw UI
   

-- renders everyting that hasn't in-game position
-- Background component would be added later
renderBackground :: SDL.Renderer -> System' ()
renderBackground h =
   cmapM_ $ \(CRenderable pic, Not :: Not CPosition) ->
      liftIO $ renderPicture h pic
   
-- quick and dirty
renderEnts :: SDL.Renderer -> Double -> System' ()
renderEnts h interpTime = do
   cmapM $ \(CRenderable pic, CPosition pos, Not :: Not CVelocity) -> do
      let pic' = pic{picPosition = P (posToRenderPos pos)}
      liftIO $ renderPicture h pic'
      return $ CRenderable pic'
   cmapM $ \(CRenderable pic, CPosition pos, CVelocity vel) -> do
      let interpPos = pos + vel * (V2 interpTime interpTime)
          pic' = pic{picPosition = P (posToRenderPos interpPos)}
      liftIO $ renderPicture h pic'
      return $ CRenderable pic'
   return ()

posToRenderPos :: V2 Double -> V2 CInt
posToRenderPos realPos = ((fromIntegral :: Integer -> CInt). floor) <$> realPos