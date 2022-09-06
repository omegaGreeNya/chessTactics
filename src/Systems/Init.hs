{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- To Do
-- Generic Entity adding.
module Systems.Init 
   (initSystem) where

import Prelude hiding (lookup)

import Control.Monad.IO.Class (liftIO)

import Apecs (newEntity_)
import Linear.Affine(Point(..))
import Linear.V2
import Linear.V4
import Stores.Cache.Class (ResourceCache(..))
import Types (GameHandle(..), AppCaches(..))
import Types.Picture
import Resource.Texture (Texture(..))

import World (System')
import World.Components

import Systems.Init.Caches (setCaches)

initSystem :: GameHandle -> System' ()
initSystem h = do
   setCaches h
   initEnts h

initEnts :: GameHandle -> System' ()
initEnts GameHandle{..} = do
   newEntity_ 
      ( CPosition (V2 0 0)
      , CVelocity (V2 (1) (1))
      , CRenderable 
         (Picture 
            (P (V2 0 0))
            (RGeometry 
               (V4 maxBound 0 0 maxBound)
               (Square (V2 10 10))
            )
         )
      )
   newEntity_
      (CRenderable (Picture (P (V2 0 0)) (RGeometry ((V4 0 0 maxBound maxBound))(Square (V2 100 1000000)))))
   cow <- liftIO $ lookup (textureCache hCaches) ".\\data\\Images\\Cow\\cow_pink.png"
   case cow of
      Nothing -> return ()
      Just (Texture cow') ->
         newEntity_   
            (CRenderable (Picture (P (V2 100 100)) (RTexture cow' Nothing)))