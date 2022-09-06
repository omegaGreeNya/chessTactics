-- | Script language for Resource Manager.
-- Resource Manager controls caches.
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.ResourceManager.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Stores.Cache.Class (ResourceCache(..), CacheableResource(..))
import Resource.Class (Resource(..))
import Types (Path)

-- | Very naive MVP resource manager language.
-- But still, there enought functional for simple caching.
-- Can be used to manipulate caches, but not initialize new one.
data ResourceManagerF next where
   LoadResource
      :: (Resource res)
      => Path -> (res -> next) -> ResourceManagerF next
   -- ^ Inits resource from external source.
   CacheResource 
      :: (ResourceCache cache) 
      => cache -> Key cache -> Val cache -> (() -> next) -> ResourceManagerF next
   -- ^ Adds loaded resource into cache
   CacheResourceFromSource 
      :: (CacheableResource cache)
      => cache -> Path -> (() -> next) -> ResourceManagerF next
   -- ^ Loads Resource right into cache. Not necessary, but surely useful!
   FlushCachedResource
      :: (ResourceCache cache, Resource (Val cache))
      => cache -> Key cache -> (() -> next) -> ResourceManagerF next
   -- ^ Frees cached resource.
   FlushCache
      :: (ResourceCache cache, Resource (Val cache))
      => cache -> (() -> next) -> ResourceManagerF next
   -- ^ Applies flush method to every resurce inside provided cache
   -- and then delets all elements from cache storage.
   ScanDirectory :: FilePath -> ([FilePath] -> next) -> ResourceManagerF next
   -- ^ Lists all FILES (no folders) inside given path and all subdirs.

instance Functor ResourceManagerF where
   fmap f (LoadResource path next) = LoadResource path (f . next)
   fmap f (CacheResource cache key val next) = CacheResource cache key val (f . next)
   fmap f (CacheResourceFromSource cache path next) = CacheResourceFromSource cache path (f . next)
   fmap f (FlushCachedResource cache key next) = FlushCachedResource cache key (f . next)
   fmap f (FlushCache cache next) = FlushCache cache (f . next)
   fmap f (ScanDirectory path next) = ScanDirectory path (f . next)

type ResourceManagerL = F ResourceManagerF

-- | Loads Resource from source.
loadResource :: (Resource res)
             => Path -> ResourceManagerL res
loadResource path = liftF $ LoadResource path id

-- | Puts loaded resource into provided cache.
cacheLoadedResource :: (ResourceCache cache)
                    => cache -> Key cache -> Val cache -> ResourceManagerL ()
cacheLoadedResource cache key val = liftF $ CacheResource cache key val id

-- | Loads resource from path and puts inside provided cache.
cacheResourceFromSource :: (CacheableResource cache)
                        => cache
                        -> Path 
                        -> ResourceManagerL ()
cacheResourceFromSource cache path = liftF $ CacheResourceFromSource cache path id

-- | Removes Resource from cache storage, and applies flush method on resource.
flushCachedReource :: (ResourceCache cache, Resource (Val cache))
                   => cache
                   -> Key cache
                   -> ResourceManagerL ()
flushCachedReource cache key = liftF $ FlushCachedResource cache key id

-- | Applies flush method to all resources inside provided cache
-- and then delets all elements inside cache storage.
flushCache :: (ResourceCache cache, Resource (Val cache))
           => cache -> ResourceManagerL ()
flushCache cache = liftF $ FlushCache cache id

scanDirectory :: FilePath -> ResourceManagerL [FilePath]
scanDirectory path = liftF $ ScanDirectory path id