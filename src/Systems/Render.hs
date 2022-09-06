{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Systems.Render
   (drawSystem) where

import GHC.Float (double2Int)

import Apecs (Not(..), cmapM_, cmapM)
import Control.Monad.IO.Class (liftIO)

import Linear.Affine (Point(..))

-- << ECS
import World (System')
import World.Components
-- >>

import Render (flush, renderPicture, present)
import Types ( GameHandle(..), SubSystemsHandle(..), SDLHandle(..)
             , Picture(..))

drawSystem :: GameHandle -> System' ()
drawSystem GameHandle{..} = drawSystem' hSystems

drawSystem' :: SubSystemsHandle -> System' ()
drawSystem' h@SubSystemsHandle{..} = do
   liftIO $ flush (hRenderer hSDL)
   -- ^ clean up
   renderBackground h
   -- ^ draw backghround
   renderEnts h
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
renderEnts :: SubSystemsHandle -> System' ()
renderEnts SubSystemsHandle{..} = 
   cmapM $ \(CRenderable pic, CPosition pos) -> do
      let pic' = pic{picPosition = P ((fromIntegral . double2Int . (*10)) <$> pos)}
      liftIO $ renderPicture (hRenderer hSDL) pic'
      return $ CRenderable pic'