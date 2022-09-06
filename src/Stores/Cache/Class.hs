{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module re-exports necesary HashMap functions from hashtable package.
-- Suggestion: Make own realization based on IntMap and LRU
-- (link: https://habr.com/ru/post/136758/)
module Stores.Cache.Class
   ( ResourceCache(..)
   , CacheableResource(..)
   , CacheErrorType(..)
   , CacheError(..)
   , cacheMissingProvidedKey
   , initializationError
   ) where

import Control.Exception (Exception(..))
import Data.Kind (Type)
import Data.Text (Text)

import qualified Data.Text as T

import Types.Game.SubSystems (SubSystemsHandle)
import Types.Path (Path(..))
import Resource.Class (Resource(..), ResourceLoadError(..))

-- | Resource Cache is the mutable storage for 
-- cached resources (Images, Sound, Entities, etc).
-- Class unifies diffirent underlying stores.

-- I know, IO is not the best option, but sure, the simpliest.
class (Show (Key cache))
   => ResourceCache cache where
   type Key cache :: Type
   type Val cache :: Type
   
   new    :: IO cache
   -- ^ It's okay, if cache wants to scan data folders or whatever.
   -- I even feel like adding SystemConfig, into GameConfig/GameHandle, or something like that.
   -- Just to make some tuning after compiling/on runtime possible.
   insert :: cache -> Key cache -> Val cache -> IO ()
   lookup :: cache -> Key cache -> IO (Maybe (Val cache))
   delete :: cache -> Key cache -> IO ()
   --fromList :: [(Key, Val)] -> IO cache
   toList :: cache -> IO [(Key cache, Val cache)]

-- | Class defines relations between resource and cache.
-- Instance indecates, that cache stores resources, that can be loaded.
-- All you need to do, is define key generation function.
-- That could be some kind of hashing, or whatever.
class ( ResourceCache cache
      , Resource (Val cache))
   => CacheableResource cache where
   constructResourceKey :: cache -> Path -> Key cache
   -- ^ Must construct UNIQUE key from UNIQUE Path.
   {-# MINIMAL constructResourceKey #-}
   -- | Loads and caches resources
   -- Maybe is as return type, but Either used to keep error handling consistent.
   cacheResourceFromPath :: SubSystemsHandle -> cache -> Path -> IO (Either ResourceLoadError ())
   
   default cacheResourceFromPath :: SubSystemsHandle -> cache -> Path -> IO (Either ResourceLoadError ())
   cacheResourceFromPath h cache path = do
      loadResult <- loadFromPath h path
      case loadResult of
         Right val -> do
            let key = constructResourceKey cache path
            insert cache key val
            return $ Right ()
         Left err -> return $ Left err

-- << move away
-- | Type of Erorr
data CacheErrorType
   = CacheMissingProvidedKey
   | InitializationError
   deriving (Show)

data CacheError = CacheError CacheErrorType Text
   deriving (Show)

instance Exception CacheError

cacheMissingProvidedKey :: (Show err) => err -> CacheError
cacheMissingProvidedKey err = CacheError CacheMissingProvidedKey (T.pack $ show err)

initializationError :: (Show err) => err -> CacheError
initializationError err = CacheError InitializationError (T.pack $ show err)