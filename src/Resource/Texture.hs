{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines Texture resurce and necessary instances.
module Resource.Texture
   ( Texture(..)
   ) where

import Control.Exception (IOException, try)

import qualified Data.Text as T
import qualified SDL ( Renderer, surfaceDimensions
                     , createTextureFromSurface
                     , destroyTexture, freeSurface)
import qualified SDL.Image as SDL (load)

import Types.Game.SubSystems (SubSystemsHandle(..), SDLHandle(..)) 
import Types.Path (Path(..))
import Types.Picture (TextureRaw)

import Resource.Class (Resource(..), ResourceLoadError, loadingError)

-- | Texture = Texture (SDL.Texture, V2 CInt)
newtype Texture = Texture TextureRaw

instance Resource Texture where
   loadFromPath SubSystemsHandle{..} (LocalPath path) = 
      loadTextureLocal (hRenderer hSDL) path
   -- | Frees texture data from GPU memory.
   flushResource (Texture (texture, _)) = 
      SDL.destroyTexture texture

-- << Move away


-- | Loads texture (SDL utility) and it's dimentions size
-- from local drive. (Safe wrapper around loadTexture')
loadTextureLocal :: SDL.Renderer -> FilePath -> IO (Either ResourceLoadError Texture)
loadTextureLocal renderer path = do
   -- loadTexture is unsafe IO function
   loadResult <- try @IOException $ loadTextureLocal' renderer path
   return $ case loadResult of
      Left err -> Left $ loadingError (T.pack $ show err)
      Right textureRaw -> Right $ Texture textureRaw

-- | Loads texture (SDL utility) and it's dimentions size from local drive.
-- UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE 
loadTextureLocal' :: SDL.Renderer -> FilePath -> IO TextureRaw
loadTextureLocal' renderer path = do
      surface <- SDL.load path
      size <- SDL.surfaceDimensions surface
      texture <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      return (texture, size)