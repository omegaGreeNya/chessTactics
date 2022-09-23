module Types.SystemsStack
   ( SubSysFPS
   , LastCallTime
   , MaxSkipFrames
   , SkippedFrames
   , SystemsList
   , SystemsStack
   ) where

import Types.Game.Handle (GameHandle)
import Types.StackMetrics (StackId)
import Types.Time (FPS, HiResTime)
import World (System')

-- | Desired sub-system executions per second.
type SubSysFPS = FPS -- :: Int

-- | The time mark of the last execution.
type LastCallTime = HiResTime

-- | The maximum amount of skipped frames
type MaxSkipFrames = Int

-- | Just a number. Current amount of skipped frames.
type SkippedFrames = Int

-- | List of systems what should be executed one after another,
-- and shares same time to proceed.
type SystemsList systemTime = [GameHandle -> systemTime -> System' ()]

-- | Stack identificator and list of systems to run)
type SystemsStack systemTime 
   = (StackId, SystemsList systemTime)