-- | Script to collect systems stack.
-- Systems stack it's systems that share same time to proceed
-- and should be runned one after another.
--
-- On that level of Language abstraction 
-- you should not bother with how-to make injection into game loop.
-- AND DEFENENTLY DO NOT, !DO NOT! make infinite/cycled/recursive/unterminatable subsystems.
-- Leave that to the higher language.

{-# LANGUAGE GADTs #-}

module Core.SubSys.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Game.Handle (GameHandle)
import World (System')

data SubSysF systemTime next where
   CreateSystemsStack :: [GameHandle -> systemTime -> System' ()]
                      -- ^ list of systems to run
                      -- Curently sustemTime may be HiResTime (as tics to proceed) or Double (for interpolation based stacks).
                      -> (() -> next)
                      -> SubSysF systemTime next
   -- ^ Just a description of system stack.

instance Functor (SubSysF systemTime) where
   fmap f (CreateSystemsStack sysList next) = CreateSystemsStack sysList (f . next)

type SubSysL systemTime = F (SubSysF systemTime)

createSystemsStack :: [GameHandle -> systemTime -> System' ()] -> SubSysL systemTime ()
createSystemsStack sysList = liftF $ CreateSystemsStack sysList id