{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
-- | ResourceManagerL script interpreters defined here.

-- TO DO!!
-- Move away implementation
-- Proper error handling. Suggestion: Use coproducts.
module Core.ResourceManager.Interpreter 
   (runResourceManager) where

import Prelude hiding (lookup)

import Control.Exception         (throwIO)
import Control.Monad.Free.Church (foldF)

import Resource.Class (Resource (..), loadingError)
import Stores.Cache.Class 
   (ResourceCache(..), CacheableResource(..), cacheMissingProvidedKey)
import Types (GameHandle(..))
import Utils.Directory (listDirectoryWithSubs)

import qualified Core.ResourceManager.Language as L

resourceManagerInterpretF :: GameHandle -> L.ResourceManagerF a -> IO a
resourceManagerInterpretF GameHandle{..} (L.LoadResource path next) = do
   result <- loadFromPath hSystems path
   case result of
      Right res -> return $ next res
      Left  err -> throwIO $ loadingError err
resourceManagerInterpretF _ (L.CacheResource cache key val next) = do
   insert cache key val
   return $ next ()
resourceManagerInterpretF GameHandle{..} (L.CacheResourceFromSource cache path next) = do
   result <- cacheResourceFromPath hSystems cache path
   case result of
      Right res -> return $ next res
      Left  err -> throwIO $ loadingError err
resourceManagerInterpretF _ (L.FlushCachedResource cache key next) = do
   result <- lookup cache key
   case result of
      Nothing -> throwIO $ cacheMissingProvidedKey key
      Just val -> do
         delete cache key
         flushResource val
         return $ next ()
resourceManagerInterpretF _ (L.FlushCache cache next) = do
   (keys, vals) <- fmap unzip $ toList cache
   mapM_ (delete cache) keys
   mapM_ flushResource vals
   return $ next ()
resourceManagerInterpretF _ (L.ScanDirectory path next) = do
   result <- listDirectoryWithSubs path
   return $ next result

runResourceManager :: GameHandle -> L.ResourceManagerL a -> IO a
runResourceManager h = foldF $ resourceManagerInterpretF h