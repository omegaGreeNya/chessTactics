module Core.SystemControl.Interpreter where

import Control.Monad (when)
import Control.Monad.Free.Church (foldF)
import qualified Apecs (runWith)
import qualified SDL.Time as SDL

import StackMetrics (getNewStackId, markStackExecution)
import Types.Game.Handle (GameHandle)
import Types.SystemsStack (SystemsStack)
import Types.Time (HiResTime)
import Utils (fpsToTics)
import World (World)

import qualified Core.SystemControl.Language as L

interpretSystemControlF :: GameHandle
                        -> World
                        -> L.SystemControlF a 
                        -> IO a
interpretSystemControlF h _ (L.CreateSystemsStack sysList next) = do
   currentTime <- SDL.ticks
   stackId <- getNewStackId h currentTime
   return $ next (stackId, sysList)
interpretSystemControlF h w (L.ApplySystem system next) = do
   result <- Apecs.runWith w $ system h
   return $ next result
interpretSystemControlF _ w (L.RunSimpleSystem system next) = do
   result <- Apecs.runWith w system
   return $ next result
interpretSystemControlF h w (L.RunStack lastCallTime mFPS sysStack next) = do
   -- putStrLn "Runned user input stack"
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
   
   when it'sTime $ do
      _ <- runSystemStack h w toProceedTime sysStack
      return ()
   return $ next currentTime

interpretSystemControlF h w
   (L.RunInterpolationStack (lastCallTime, fps, fstStack) (skippedFrames, maxSkipFrames, sndStack) next) = do
   -- putStrLn "Runned interpolation Stack"
   currentTime <- SDL.ticks
   let ticksToProceed = fpsToTics fps
       deltaTime = currentTime - lastCallTime
       it'sTime = deltaTime >= ticksToProceed
   
   lastCallTime' <-
   -- If we execute fst system, then we update lastCallTime.
   -- If not, then lastCallTime stays the same.
      if it'sTime 
      then do
         (startTime, _) <- runSystemStack h w ticksToProceed fstStack
         -- putStrLn "Runned fst sys"
         return startTime
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
            -- putStrLn "Runned snd sys"
            return 0
         else
            return $ skippedFrames + 1
   
   return $ next (lastCallTime', skippedFrames')
interpretSystemControlF h w (L.RunSystemLoop conditionSys initState stepScript next) = do
   let loop loopState = do
         running <- Apecs.runWith w conditionSys
         when running $ do
            loopState' <- runSystemController h w (stepScript loopState)
            --drawMetrics h
            loop loopState'
         return ()
   _ <- loop initState
   return $ next ()

runSystemController :: GameHandle -> World -> L.SystemControlL a -> IO a
runSystemController h w = foldF $ interpretSystemControlF h w

-- << Helper functions

-- | Executes systemsStack writes metrics if possible and returns execution START time
runSystemStack :: GameHandle -> World -> systemTime -> SystemsStack systemTime -> IO (HiResTime, HiResTime)
runSystemStack h w time (stackId, systemsList) = do
   startTime <- SDL.ticks
   markStackExecution h stackId startTime
   let f system = Apecs.runWith w $ system h time
   mapM_ f systemsList
   endTime <- SDL.ticks
   return (startTime, endTime)
-- >>