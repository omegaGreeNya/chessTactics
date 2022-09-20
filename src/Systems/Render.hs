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
import Types ( GameHandle(..), SubSystemsHandle(..), SDLHandle(..)
             , Picture(..))

drawSystem :: GameHandle -> Double -> System' ()
drawSystem GameHandle{..} interpTime = drawSystem' hSystems interpTime

drawSystem' :: SubSystemsHandle -> Double -> System' ()
drawSystem' h@SubSystemsHandle{..} interpTime = do
   liftIO $ flush (hRenderer hSDL)
   -- ^ clean up
   renderBackground h
   -- ^ draw backghround
   renderEnts h interpTime
   -- ^ draw actors
   --renderUI
   -- ^ draw UI
   liftIO $ present (hRenderer hSDL)

-- renders everyting that hasn't in-game position
-- Background component would be added later
renderBackground :: SubSystemsHandle -> System' ()
renderBackground SubSystemsHandle{..} =
   cmapM_ $ \(CRenderable pic, Not :: Not CPosition) ->
      liftIO $ renderPicture (hRenderer hSDL) pic
   
-- quick and dirty
renderEnts :: SubSystemsHandle -> Double -> System' ()
renderEnts SubSystemsHandle{..} interpTime = do
   cmapM $ \(CRenderable pic, CPosition pos, Not :: Not CVelocity) -> do
      let pic' = pic{picPosition = P (posToRenderPos pos)}
      liftIO $ renderPicture (hRenderer hSDL) pic'
      return $ CRenderable pic'
   cmapM $ \(CRenderable pic, CPosition pos, CVelocity vel) -> do
      let interpPos = pos + vel * (V2 interpTime interpTime)
          pic' = pic{picPosition = P (posToRenderPos interpPos)}
      liftIO $ renderPicture (hRenderer hSDL) pic'
      return $ CRenderable pic'
   return ()

posToRenderPos :: V2 Double -> V2 CInt
posToRenderPos realPos = ((fromIntegral :: Integer -> CInt). floor) <$> realPos