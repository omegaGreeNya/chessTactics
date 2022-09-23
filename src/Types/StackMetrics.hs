module Types.StackMetrics
   ( StackMetricsHandle(..)
   , StackMetrics(..)
   , StackPulse(..)
   , StackId
   , Pulse
   ) where

import Data.Vector.Mutable (IOVector)
import Data.IntMap.Strict (IntMap)
import Data.IORef (IORef)

import Types.Time (HiResTime)

-- | Unique Id type.
type StackId = Int

-- | Mutable vector wich stores maximal eps (index 0) and last 100 calculated eps.
-- Eps - executions per second.
type Pulse = IOVector Double

data StackPulse = StackPulse
   { pulse       :: Pulse
   , pulseCursor :: IORef Int
   -- ^ Current vector position to fill up. 1 <= pulseCursor <= 100
   }

-- | Contains timings of last two calls.
data StackMetrics = StackMetrics
   { stackLastCallTime :: IORef HiResTime
   -- ^ last execution hi-res time.
   , stackPulse        :: StackPulse
   }

-- | Map from stack id to it's mutable statistic.
type MetricsMap = IntMap StackMetrics

data StackMetricsHandle = StackMetricsHandle
   { hMetricsMap :: IORef MetricsMap
   -- ^ Mutable metrics catalog.
   , hLastId     :: IORef StackId
   -- ^ The counter of added StackMetrics.
   }
