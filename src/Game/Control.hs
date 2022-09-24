-- |
{-# LANGUAGE RecordWildCards #-}
module Game.Control
   ( gameSystemsRunner
   ) where

import Systems (isGameOver, initSystem)
import Language
import Types.SystemsStack (LastCallTime, SkippedFrames)

import Game.SystemsStack (createDrawStack, createStepStack, createUserInputStack)

data LoopState = LoopState
   { lastStepTime :: LastCallTime -- Word32
   , skippedDraws :: SkippedFrames -- Int
   }

gameSystemsRunner :: SystemControlL ()
gameSystemsRunner = do
   -- << Filling world with entities and globals.
   applySystem initSystem
   -- >> 
   
   -- << Creating SystemsStacks
   stepStack      <- createStepStack 
   drawStack      <- createDrawStack
   userInputStack <- createUserInputStack
   -- >>
   
   -- << Main GameLoop
   runSystemLoop (isGameOver >>= return . not) initLoopState $ \LoopState{..} -> do
      _ <- runStack 0 (Just 60) userInputStack
      -- ^ Just to flush event queue
      (lastStepTime', skippedDraws') <- 
         runInterpolationStack 
            (lastStepTime, 50, stepStack)
            (skippedDraws, 5, drawStack)
      return $ LoopState lastStepTime' skippedDraws'
   -- >>
   return ()

initLoopState :: LoopState
initLoopState = LoopState 0 0