module Core.SystemControl.Interpreter where

import Control.Monad (when)
import Control.Monad.Free.Church (foldF)
import qualified Apecs (runWith)
import qualified SDL.Time as SDL

import Types.Game.Handle (GameHandle)
import Utils (fpsToTics)
import World (World)

import Core.SubSys.Interpreter (runSystemStack)
import qualified Core.SystemControl.Language as L

interpretSystemControlF :: GameHandle
                        -> World
                        -> L.SystemControlF a 
                        -> IO a
interpretSystemControlF _ w (L.RunSimpleSystem system next) = do
   result <- Apecs.runWith w system
   return $ next result
interpretSystemControlF h w (L.RunStack lastCallTime mFPS sysStack next) = do
   currentTime <- SDL.ticks
   let deltaTime = currentTime - lastCallTime
       (it'sTime, toProceedTime) =
         maybe 
            (True, deltaTime)
            -- ^ If FPS not specified we always run systemStack
            (\fps -> let ticksToProceed = fpsToTics fps 
                     in (deltaTime >= ticksToProceed, ticksToProceed))
            -- ^ Otherwise, we run systemStack no more often than fps per sec.
            mFPS
   
   when it'sTime $ 
      runSystemStack h w toProceedTime sysStack
   return $ next currentTime
   
interpretSystemControlF h w
   (L.RunInterpolationStack (lastCallTime, fps, fstStack) (skippedFrames, maxSkipFrames, sndStack) next) = do
   currentTime <- SDL.ticks
   let ticksToProceed = fpsToTics fps
       deltaTime = currentTime - lastCallTime
       it'sTime = deltaTime >= ticksToProceed
   
   lastCallTime' <- 
   -- If we execute fst system, then we update lastCallTime.
   -- If not, then lastCallTime stays the same.
      if it'sTime 
      then do
         _ <- runSystemStack h w ticksToProceed fstStack
         SDL.ticks
      else
         return lastCallTime
   
   skippedFrames' <-
      -- Same Here. If we execute snd System we reset skippedFrames counter.
      -- If not, then we increase that counter by 1.
      if (currentTime - lastCallTime' < ticksToProceed 
         -- We have spare time between fst system calls.
         || skippedFrames >= maxSkipFrames)
         -- OR, we alredy skiped maximum amount of snd system executions.
         then do
            currentTime' <- SDL.ticks
            let interpolationTime = (fromIntegral $ currentTime' - lastCallTime')/(fromIntegral ticksToProceed)
            _ <- runSystemStack h w interpolationTime sndStack
            return 0
         else
            return $ skippedFrames + 1
   
   return $ next (lastCallTime', skippedFrames')

runOneStepOfGameLoop :: GameHandle -> World -> L.SystemControlL a -> IO a
runOneStepOfGameLoop h w = foldF $ interpretSystemControlF h w