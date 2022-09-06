module Resource.Class
   ( Resource(..)
   , ResourceLoadErrorType(..)
   , ResourceLoadError(..)
   , canNotReadFile
   , loadingError
   ) where

import Control.Exception (Exception(..))
import Data.Text (Text)

import qualified Data.Text as T

import Types.Game.SubSystems (SubSystemsHandle)
import Types.Path (Path(..))


-- | Resource is the data, that can be loaded. That's all.
-- Class defines two methods for loading Resource from external source,
-- and for freeing resource from memory.
-- (Hardware memory, like GPU. Just return () if it's not the case for your Resource).
class Resource res where
   -- ^ Maybe just FilePath, or Word for hash, or Int, whatever suits your needs.
   loadFromPath :: SubSystemsHandle -> Path -> IO (Either ResourceLoadError res)
   -- ^ Loads resource from FilePath, Url.. Or fails.
   flushResource :: res -> IO ()
   -- ^ Frees resource from hardware memory.

-- << Move away
data ResourceLoadErrorType
   = CanNotReadFile
   | LoadingError
   deriving (Show)

data ResourceLoadError 
   = ResourceLoadError ResourceLoadErrorType Text
   deriving (Show)

instance Exception ResourceLoadError

canNotReadFile :: (Show err) => err -> ResourceLoadError
canNotReadFile err = ResourceLoadError CanNotReadFile (T.pack $ show err)

loadingError :: (Show err) => err -> ResourceLoadError
loadingError err = ResourceLoadError LoadingError (T.pack $ show err)