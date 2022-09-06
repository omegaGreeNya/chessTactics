-- | Utility functions related to filepath, folders, files, etc.
module Utils.Directory
   ( getDirectoryElementsCount
   , listDirectoryWithSubs
   ) where

import System.Directory (listDirectory)
import System.FilePath (hasExtension, (</>))

-- | Unsafe. Counts how many FILES inside specified dir and all subdirs.
getDirectoryElementsCount :: FilePath -> IO Int
getDirectoryElementsCount path =
   listDirectoryWithSubs path >>= return . length

-- | List filepaths of all files inside provided filepaths and subdirs.
-- Returning filepaths if "absolute" relative to the topPath.
-- For example, listDirectoryWithSubs "path" == ["path\file.ext", "path\subdir\file.ext" ..]
listDirectoryWithSubs :: FilePath -> IO [FilePath]
listDirectoryWithSubs topPath =
   if hasExtension topPath
   then
      return [topPath]
   else do
      result <- listDirectory topPath
      let fulltopPathResult = map (topPath </>) result
      fmap concat $ 
         mapM listDirectoryWithSubs fulltopPathResult