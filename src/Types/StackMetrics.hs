module Types.StackMetrics
   ( StackMetricsHandle(..)
   , StackMetrics(..)
   , StackId
   ) where

import Data.IntMap (IntMap)
import Data.IORef (IORef)

import Types.Time (HiResTime)

-- | Unique Id type.
type StackId = Int

-- | Contains timings of last two calls.
data StackMetrics = StackMetrics
   { stackLastCallTime :: HiResTime
   -- ^ last execution hi-res time.
   , stackPrevCallTime :: HiResTime
   -- ^ previous execution hi-res time.
   } deriving (Show)

-- | IntMap :)
type MetricsMap = IntMap (IORef StackMetrics)

data StackMetricsHandle = StackMetricsHandle
   { hMetricsMap :: IORef MetricsMap
   -- ^ Mutable metrics catalog.
   , hLastId     :: IORef StackId
   -- ^ Last added id (0 means that there is currenly no metrics).
   }
