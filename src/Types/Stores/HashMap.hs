-- | Module contains wrapped HashTable data type.
-- Suggestion: Make own realization based on IntMap and LRU
-- (link: https://habr.com/ru/post/136758/)
--    Pros: caching key hash.
module Types.Stores.HashMap
   ( HashMap
   ) where

import Data.HashTable.IO (BasicHashTable)

-- | Mutable map, based on basic hashmap from hashtables package.
type HashMap key val
   = BasicHashTable key val