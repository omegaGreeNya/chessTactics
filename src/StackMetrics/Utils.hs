{-# LANGUAGE RecordWildCards #-}
-- | Module defines StackMetrics,
-- Handle and methods to tack stacks execution timings.
module StackMetrics.Utils
   ( StackMetricsHandle
   , StackMetrics
   , StackPulse
   , StackId
   , withStackMetricsHandle
   , initStackMetrics
   , getNewStackId'
   , getNewStackId
   , markStackExecution'
   , markStackExecution
   , getStackPulse
   , getStackPulses
   , unwrapStackPulse
   ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as Map
import qualified Data.Vector.Mutable as V

import Constants (epsHistoryLength)
import Types.Game.Handle (GameHandle(..))
import Types.Game.SubSystems (SubSystemsHandle(..))
import Types.StackMetrics ( StackMetrics(..), StackMetricsHandle(..)
                          , StackPulse(..), StackId)
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
   
   stackLastCallTime <- newIORef callTime 
   
   clearPulse <- V.replicate epsHistoryLength 0 -- fst elem has index 0
   cursor <- newIORef 1 -- 1 is the fst position to write eps.
   let stackPulse = StackPulse clearPulse cursor
       newMetrics = StackMetrics{..}
   
   writeIORef hMetricsMap $ Map.insert newId newMetrics metricsMap
   return newId

-- | GameHandle version. Always returns 0 if StackMetricsHandle not defined.
getNewStackId :: GameHandle -> HiResTime -> IO StackId
getNewStackId h callTime = do
   mId <- withStackMetricsHandle h (flip getNewStackId' $ callTime)
   return $ maybe 0 id mId

-- | Lookups stack metrics by StackId
getStackMetrics :: StackMetricsHandle -> StackId -> IO (Maybe StackMetrics)
getStackMetrics StackMetricsHandle{..} stackId = do
   metricsMap <- readIORef hMetricsMap
   return $ Map.lookup stackId metricsMap

-- | '1 sec / time between last two cals'
calculateExecsPerSec :: HiResTime -- ^ Last call time
                     -> HiResTime -- ^ Prev call time
                     -> Double    -- ^ Resulted eps
calculateExecsPerSec stackLastCallTime stackPrevCallTime =
   let timeDiff = stackLastCallTime - stackPrevCallTime
   in if timeDiff == 0
      then 1
      else 1000 / (fromIntegral timeDiff)

-- | Updates provided pulse with new eps.
updatePulse :: Double         -- ^ eps to add.
            -> StackPulse     -- ^ Pulse.
            -> IO ()
updatePulse eps StackPulse{..} = do
   cursorPos <- readIORef pulseCursor
   
   V.modify pulse (const eps) cursorPos         -- add eps to history
   V.modify pulse                               -- update max
            (\x -> max x eps) 
            0 
   
   let newPos = 
         if cursorPos == (epsHistoryLength - 1)
         then 0 
         else cursorPos + 1
   
   writeIORef pulseCursor newPos

-- | Updates provided metrics with new execution time
updateStackMetrics :: HiResTime -> StackMetrics -> IO ()
updateStackMetrics callTime StackMetrics{..} = do
   prevCallTime <- readIORef stackLastCallTime
   let eps = calculateExecsPerSec callTime prevCallTime
   
   writeIORef stackLastCallTime callTime
   -- ^ Update last call time
   updatePulse eps stackPulse
   -- ^ Pump pulse

-- | If provided id exist, then writes metrics.
-- Otherwise does nothing.
markStackExecution' :: StackMetricsHandle -> StackId -> HiResTime -> IO ()
markStackExecution' h stackId callTime = do
   mMetrics <- getStackMetrics h stackId
   case mMetrics of
      Just stackMetrics -> updateStackMetrics callTime stackMetrics
      _                 -> return ()

-- | GameHandle wrapper version.
markStackExecution :: GameHandle -> StackId -> HiResTime -> IO ()
markStackExecution h stackId callTime = 
   withStackMetricsHandle h (\smH -> markStackExecution' smH stackId callTime)
   >> return ()

-- | 
getStackPulse :: StackMetricsHandle -> StackId -> IO (Maybe StackPulse)
getStackPulse h stackId = do
   mMetrics <- getStackMetrics h stackId
   return $ 
      case mMetrics of
         Just stackMetrics -> Just $ stackPulse stackMetrics
         _                 -> Nothing

getStackPulses :: StackMetricsHandle -> IO ([(StackId, StackPulse)])
getStackPulses StackMetricsHandle{..} = do
   metricsMap <- readIORef hMetricsMap
   return . map (fmap stackPulse) $ Map.toList metricsMap

-- | Unwraps StackPulse into pair of max eps in history
-- and all last epsHistoryLength eps in history
unwrapStackPulse :: StackPulse -> IO (Double, [Double])
unwrapStackPulse StackPulse{..} = do
   maxEps <- V.read pulse 0
   
   cursor <- readIORef pulseCursor
   let getNext x = if x > 1 then (x - 1) else (epsHistoryLength - 1)
       reader :: Int -> IO [Double]
       reader n = do
         eps <- V.read pulse n
         let n' = getNext n
         if n' == getNext cursor
         then return [eps]
         else do
            history <- reader n'
            return $ eps : history 
   pulseList <- reader $ getNext cursor
   return (maxEps, pulseList)