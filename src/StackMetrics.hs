{-# LANGUAGE RecordWildCards #-}
-- | Module defines StackMetrics,
-- Handle and methods to tack stacks execution timings.
module StackMetrics
   ( StackMetricsHandle
   , StackMetrics
   , StackId
   , initStackMetrics
   , getNewStackId
   , markStackExecution
   --, drawMetrics
   ) where

import StackMetrics.Utils
       ( StackMetricsHandle
       , StackMetrics
       , StackId
       , initStackMetrics
       , getNewStackId
       , markStackExecution
       --, drawMetrics
       )

{-
import StackMetrics.Utils
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
         )

import Types.Game.Handle (GameHandle(..))
import Types.Game.SubSystems (SubSystemsHandle(..))


drawMetrics' :: StackMetrics -> IO ()
drawMetrics' StackMetrics = do

drawMetrics :: GameHandle -> IO ()
drawMetrics h = with 
-}