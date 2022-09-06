{-# LANGUAGE RecordWildCards #-}
-- | ResourceManager scripts.

-- TO DO
-- Stop ignoring errors in your script, 
-- or move error handling OUTSIDE the script =)
module Game.ResourceManager
   ( fillCaches
   , flushCaches
   ) where

import Language
import Stores.Cache.Class (CacheableResource(..))
import Types (GameHandle(..), AppCaches(..), Path(..))

import Constants (textureFolder)

fillCaches :: GameHandle -> ResourceManagerL ()
fillCaches GameHandle{..} = do
   cacheAllFiles (textureCache hCaches) textureFolder

-- | Tries to load all Files inside provided dir into provided cache.
cacheAllFiles :: (CacheableResource cache) 
              => cache -> FilePath -> ResourceManagerL ()
cacheAllFiles cache path = do
   paths <- scanDirectory path
   mapM_ (cacheResourceFromSource cache) $ map LocalPath paths

flushCaches :: GameHandle -> ResourceManagerL ()
flushCaches GameHandle{..} = do
   flushCache (textureCache hCaches)