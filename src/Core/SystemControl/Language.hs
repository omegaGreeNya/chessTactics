-- | Script Langeage do define one step of game loop.
{-# LANGUAGE GADTs #-}
module Core.SystemControl.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Time (HiResTime)
import Types.SystemsStack (SubSysFPS, LastCallTime, MaxSkipFrames, SkippedFrames)
import World (System')

import qualified Core.SubSys.Language as L

data SystemControlF next where
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
            -> L.SubSysL HiResTime ()
            -- ^ Systems Stack.
            -> (LastCallTime -> next) 
            -> SystemControlF next
   -- ^ Runs systems stack with fixed fps, or on every loop step.
   
   RunInterpolationStack :: (LastCallTime, SubSysFPS, L.SubSysL HiResTime ())
                         -- ^ (The last call of this systemsStack, Subsystem desired executions per second, subsystems stack)
                         -> (SkippedFrames, MaxSkipFrames, L.SubSysL Double ())
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

instance Functor SystemControlF where
   fmap f (RunSimpleSystem sys next) = RunSimpleSystem sys (f . next)
   fmap f (RunStack lastCallTime mFps script next) = RunStack lastCallTime mFps script (f . next)
   fmap f (RunInterpolationStack fstSys sndSys next) = RunInterpolationStack fstSys sndSys (f . next)

type SystemControlL = F SystemControlF

runSimpleSystem :: System' a -> SystemControlL a
runSimpleSystem sys = liftF $ RunSimpleSystem sys id

runStack :: HiResTime -> Maybe SubSysFPS -> L.SubSysL HiResTime () -> SystemControlL LastCallTime
runStack lastCallTime mFps script = liftF $ RunStack lastCallTime mFps script id

runInterpolationStack :: (LastCallTime, SubSysFPS, L.SubSysL HiResTime ()) -> (SkippedFrames, MaxSkipFrames, L.SubSysL Double ()) -> SystemControlL (LastCallTime, SkippedFrames)
runInterpolationStack fstSys sndSys = liftF $ RunInterpolationStack fstSys sndSys id