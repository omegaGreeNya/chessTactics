{-# LANGUAGE RecordWildCards #-}
-- | Module defines StackMetrics,
-- Handle and methods to tack stacks execution timings.
module StackMetrics.Utils
   ( StackMetricsHandle
   , StackMetrics
   , StackId
   , withStackMetricsHandle
   , initStackMetrics
   , getNewStackId'
   , getNewStackId
   , getStackExecPerSec'
   , getStackExecPerSec
   , markStackExecution'
   , markStackExecution
   ) where

import Data.IORef ( IORef, newIORef, readIORef
                  , writeIORef, modifyIORef)
import qualified Data.IntMap as Map

import Types.Game.Handle (GameHandle(..))
import Types.Game.SubSystems (SubSystemsHandle(..))
import Types.StackMetrics (StackMetrics(..), StackMetricsHandle(..), StackId)
import Types.Time (HiResTime)



-- | Creates empty handle.
initStackMetrics :: IO StackMetricsHandle
initStackMetrics = do
   hLastId <- newIORef 0
   hMetricsMap <- newIORef mempty
   return StackMetricsHandle{..}

-- | Handle helper
withStackMetricsHandle :: GameHandle -> (StackMetricsHandle -> IO a) -> IO (Maybe a)
withStackMetricsHandle GameHandle{..} action = do
   case hMetrics hSystems of
      Just h -> fmap Just $ action h
      _ -> return Nothing


-- | Writes new stackMetrics and returns it's unique Id.
getNewStackId' :: StackMetricsHandle -> HiResTime -> IO StackId
getNewStackId' StackMetricsHandle{..} callTime = do
   lastId <- readIORef hLastId
   let newId = lastId + 1
   writeIORef hLastId newId
   metricsMap <- readIORef hMetricsMap
   newMetrics <- newIORef $ StackMetrics callTime callTime
   writeIORef hMetricsMap $ Map.insert newId newMetrics metricsMap
   return newId

-- | GameHandle version. Always returns 0 if StackMetricsHandle not defined.
getNewStackId :: GameHandle -> HiResTime -> IO StackId
getNewStackId h callTime = do
   mId <- withStackMetricsHandle h (flip getNewStackId' $ callTime)
   return $ maybe 0 id mId

-- | Lookups StackMetricsRef by StackId
getStackMetricsRef :: StackMetricsHandle -> StackId -> IO (Maybe (IORef StackMetrics))
getStackMetricsRef StackMetricsHandle{..} stackId = do
   metricsMap <- readIORef hMetricsMap
   return $ Map.lookup stackId metricsMap

-- | Sets prev call time as last call time,
-- and sets last call time as provided time.
updateStackMetrics :: HiResTime -> StackMetrics -> StackMetrics
updateStackMetrics callTime StackMetrics{..} = 
   StackMetrics callTime stackLastCallTime

-- | If provided id exist, then writes metrics.
-- Otherwise does nothing.
markStackExecution' :: StackMetricsHandle -> StackId -> HiResTime -> IO ()
markStackExecution' h stackId callTime = do
   mMetricsRef <- getStackMetricsRef h stackId
   case mMetricsRef of
      Just metricsRef -> modifyIORef metricsRef (updateStackMetrics callTime)
      _               -> return ()

-- | GameHandle wrapper version.
markStackExecution :: GameHandle -> StackId -> HiResTime -> IO ()
markStackExecution h stackId callTime = 
   withStackMetricsHandle h (\smH -> markStackExecution' smH stackId callTime)
   >> return ()


-- | Lookups stack metrics by StackId
getStackMetrics :: StackMetricsHandle -> StackId -> IO (Maybe StackMetrics)
getStackMetrics h stackId = do
   mMetricsRef <- getStackMetricsRef h stackId
   case mMetricsRef of
      Just metricsRef -> fmap Just $ readIORef metricsRef
      _               -> return Nothing

-- | '1 sec / time between last two cals'
calculateExecsPerSec :: StackMetrics -> Double
calculateExecsPerSec StackMetrics{..} =
   let timeDiff = stackLastCallTime - stackPrevCallTime
   in if timeDiff == 0
      then 1
      else 1000 / (fromIntegral timeDiff)

-- | Returns 0 if stack not found.
-- Otherwise returns '1 sec / time between last two calls`
getStackExecPerSec' :: StackMetricsHandle -> StackId -> IO Double
getStackExecPerSec' h stackId = do
   mMetrics <- getStackMetrics h stackId
   return $ maybe 0 calculateExecsPerSec mMetrics

getStackExecPerSec :: GameHandle -> StackId -> IO (Maybe Double)
getStackExecPerSec h stackId = withStackMetricsHandle h (flip getStackExecPerSec' $ stackId)
