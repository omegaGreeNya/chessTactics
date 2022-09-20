-- |
{-# LANGUAGE RecordWildCards #-}
module Game.Loop
   ( loopStepScript
   , initLoopState
   ) where

import Language
import Types.SystemsStack (LastCallTime, SkippedFrames)

import Game.SystemsStack (userInputStack, stepStack, drawStack)

data LoopState = LoopState
   { lastStepTime :: LastCallTime -- Word32
   , skippedDraws :: SkippedFrames -- Int
   }


-- Explicit state tossing can be hided behind the script.
-- All that needed for this, is defined mutable loop State.
loopStepScript :: LoopState -> SystemControlL LoopState
loopStepScript LoopState{..} = do
   _ <- runStack 0 (Just 60) userInputStack
   (lastStepTime', skippedDraws') <- 
      runInterpolationStack 
         (lastStepTime, 50, stepStack)
         (skippedDraws, 5, drawStack)
   return $ LoopState lastStepTime' skippedDraws'


initLoopState :: LoopState
initLoopState = LoopState 0 0