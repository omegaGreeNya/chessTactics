{-# LANGUAGE TypeFamilies #-}
-- | Module defines Cache storage for Texture resource.
module Stores.Cache.Texture
   ( TextureCache(..)
   , TextureId
   ) where

import Constants (textureFolder)
import Stores.Cache.Class 
   (ResourceCache(..), CacheableResource(..))
import Resource (Texture(..))
import Types.Path (Path(..))
import Utils (getDirectoryElementsCount)

import qualified Stores.HashMap as H
   (HashMap, newSized, delete, lookup, insert, toList)

-- | TextureId it's the FilePath to resource.
-- Suggestion: Store id as pair of FilePath and Hash.
--    Just to avoid recalculating hash on every call.
type TextureId = FilePath

-- | Mutable HashMap with Texture as value, and it's FilePath as key.
newtype TextureCache = TextureCache (H.HashMap TextureId Texture)

instance ResourceCache TextureCache where
   type Key TextureCache = TextureId
   type Val TextureCache = Texture
   -- We create cache with size at least as much images we have.
   -- Not the best in terms of memory usage solution. But ok for now.
   new    = newTextureCache
   insert = \(TextureCache cache) -> H.insert cache
   lookup = \(TextureCache cache) -> H.lookup cache
   delete = \(TextureCache cache) -> H.delete cache
   toList = \(TextureCache cache) -> H.toList cache

instance CacheableResource TextureCache where
   -- | We using FilePath as Cache key. (see suggestion ((eyes up)))
   constructResourceKey _ (LocalPath path) = path

newTextureCache :: IO TextureCache
newTextureCache = do
      size <- getDirectoryElementsCount textureFolder
      fmap TextureCache $ H.newSized size