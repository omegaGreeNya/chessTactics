-- | Script Langeage do define one step of game loop.
{-# LANGUAGE GADTs #-}
module Core.SystemControl.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Game.Handle (GameHandle)
import Types.Time (HiResTime)
import Types.SystemsStack ( SubSysFPS, LastCallTime, MaxSkipFrames
                          , SkippedFrames, SystemsList, SystemsStack)
import World (System')

data SystemControlF next where
   CreateSystemsStack :: SystemsList systemTime
                      -- ^ Curently systemTime may be HiResTime (as tics to proceed) 
                      -- or Double (for interpolation based stacks).
                      -> (SystemsStack systemTime -> next)
                      -> SystemControlF next
   -- ^ 
   ApplySystem :: (GameHandle -> System' a) -> (a -> next) -> SystemControlF next
   -- ^ Applies system to world. Useful to apply init systems outside the loop.
   RunSimpleSystem :: System' a -> (a -> next) -> SystemControlF next
   -- ^ Run simple systems, like one that makes a decision based on world state.
   -- It's called 'simple' because it doesn't depends on GameHandle, therefore it can't do much.
   -- It would be preferred, if such sistem doesn't affect world state, but feel free if you need to.
   
   RunStack :: LastCallTime
            -- ^ The last call of this systemsStack.
            -> Maybe SubSysFPS 
            -- ^ Just if subsystem should not exceed specific framerate.
            -- Nothing, if we want to execute subsystem as much time as we can per second.
            --
            -- In case of fixed FPS system will always get constant time to proceed.
            -- Otherwise it will get time to proceed since the last call.
            -> SystemsStack HiResTime
            -- ^ Systems Stack.
            -> (LastCallTime -> next) 
            -> SystemControlF next
   -- ^ Runs systems stack with fixed fps, or on every loop step.
   
   RunInterpolationStack :: (LastCallTime, SubSysFPS, SystemsStack HiResTime)
                         -- ^ (The last call of this systemsStack, Subsystem desired executions per second, subsystems stack)
                         -> (SkippedFrames, MaxSkipFrames, SystemsStack Double)
                         -- ^ ( Current amount of skipped execution of second systemsStack.
                         --   , Maximum amount of skipped execution of this subsystem due too long 
                         --      execution of 1st SysemsStack (MaxSkipFrames >= 0)
                         --   , Subsystem that will work with interpolation (interpolation = time consumed by 1st sys/(1/fps))).
                         -> ((LastCallTime, SkippedFrames) -> next)
                         -> SystemControlF next
   -- ^ It's bundle of two subsystem stacks.
   -- First one would be executed at fixed, defined fps,
   -- Second one, would receive interpolation time (0 <= t @Double <= 1) as time to proceed,
   -- and would be executed as many times as possible before another execution of Fst one.
   --
   -- Consider this example: 1st system changes position, velocity of the ball, and second one
   -- draws this ball on the screen, we want to run ball position logic at fixed rate, but get as
   -- many draw framerate as possible. So, that exactly use case of this method.
   --
   -- More to read: https://dewitters.com/dewitters-gameloop/
   -- https://habr.com/ru/post/136878/ (Translation)
   -- https://www.programmersought.com/article/25047833854/
   
   RunSystemLoop :: System' Bool
                 -- ^ While this system applied on World returns True, execution continues.
                 -> loopState
                 -- ^ Any loop metadata (lastCallTimes, skippedFrames, etc.)
                 -> (loopState -> SystemControlL loopState)
                 -- ^ Script that defines one step of a loop.
                 -> (() -> next)
                 -> SystemControlF next
   -- ^ Runs system in a loop. Basicly the thing that you can call main game loop.

instance Functor SystemControlF where
   fmap f (CreateSystemsStack sysList next) = CreateSystemsStack sysList (f . next)
   fmap f (ApplySystem sys next) = ApplySystem sys (f . next)
   fmap f (RunSimpleSystem sys next) = RunSimpleSystem sys (f . next)
   fmap f (RunStack lastCallTime mFps sysStack next) = RunStack lastCallTime mFps sysStack (f . next)
   fmap f (RunInterpolationStack fstSysStack sndSysStack next) = RunInterpolationStack fstSysStack sndSysStack (f . next)
   fmap f (RunSystemLoop condition stepScript initState next) = RunSystemLoop condition stepScript initState (f . next)

type SystemControlL = F SystemControlF

createSystemsStack :: SystemsList systemTime -> SystemControlL (SystemsStack systemTime)
createSystemsStack sysList = liftF $ CreateSystemsStack sysList id

applySystem :: (GameHandle -> System' a) -> SystemControlL a
applySystem sys = liftF $ ApplySystem sys id

runSimpleSystem :: System' a -> SystemControlL a
runSimpleSystem sys = liftF $ RunSimpleSystem sys id

runStack :: HiResTime -> Maybe SubSysFPS -> SystemsStack HiResTime -> SystemControlL LastCallTime
runStack lastCallTime mFps sysStack = liftF $ RunStack lastCallTime mFps sysStack id

runInterpolationStack :: (LastCallTime, SubSysFPS, SystemsStack HiResTime) -> (SkippedFrames, MaxSkipFrames, SystemsStack Double) -> SystemControlL (LastCallTime, SkippedFrames)
runInterpolationStack fstSysStack sndSysStack = liftF $ RunInterpolationStack fstSysStack sndSysStack id

runSystemLoop :: System' Bool -> loopState -> (loopState -> SystemControlL loopState) -> SystemControlL ()
runSystemLoop condition initState stepScript = liftF $ RunSystemLoop condition initState stepScript id