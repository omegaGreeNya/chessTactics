-- | Module re-exports necesary HashMap functions from hashtable package.
-- Suggestion: Make own realization based on IntMap and LRU
-- (link: https://habr.com/ru/post/136758/)
module Stores.HashMap
   ( module X
   ) where

import Data.HashTable.IO as X
   (new, newSized, delete, lookup, insert, toList)

import Types.Stores.HashMap as X
   (HashMap)