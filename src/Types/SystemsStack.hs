module Types.SystemsStack
   ( SubSysFPS
   , LastCallTime
   , MaxSkipFrames
   , SkippedFrames
   ) where

import Types.Time (FPS, HiResTime)

-- | Desired sub-system executions per second.
type SubSysFPS = FPS -- :: Int

-- | The time mark of the last execution.
type LastCallTime = HiResTime

-- | The maximum amount of skipped frames
type MaxSkipFrames = Int

-- | Just a number. Current amount of skipped frames.
type SkippedFrames = Int

