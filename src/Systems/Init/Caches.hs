{-# LANGUAGE RecordWildCards #-}
module Systems.Init.Caches
   (setCaches) where

import Apecs (set, global)

import Stores.Cache.Texture (TextureCache)
import Types (GameHandle(..), AppCaches(..))

import World (System')
import World.Components (CTextureCache(..))

setCaches :: GameHandle -> System' ()
setCaches GameHandle{..} = setCaches' hCaches

-- | Sets all initializated caches into World
setCaches' :: AppCaches -> System' ()
setCaches' AppCaches{..} = do
   setTextureCache textureCache
   -- setAnimationCache ..

-- | Adds World global component - TextureCache
setTextureCache :: TextureCache -> System' ()
setTextureCache = set global . CTextureCache