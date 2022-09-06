-- | Mocule contains global App data types.
module Types.App where

import Stores.Cache.Texture (TextureCache)

data AppCaches = AppCaches
   { textureCache :: TextureCache
   }