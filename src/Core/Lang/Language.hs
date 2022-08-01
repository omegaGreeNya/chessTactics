-- | Script monad to manipulate Game by applying Systems
{-# LANGUAGE GADTs #-}
-- | TO DO
-- Add thread forking
module Core.Lang.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Game.Handle (GameHandle)
import World (World, System')

data LangF next where
   InitWorld :: (World -> next) -> LangF next
   -- ^ Inits World record with component maps.
   ApplySystem :: World -> (GameHandle -> System' a) -> (a -> next) -> LangF next
   -- ^ Applies system to world.
   ForkSystem :: World -> (GameHandle -> System' a) -> (a -> next) -> LangF next
   -- ^ Applies system in different thread 
   -- useful for launching many loop-systems
   -- WIP LoadResources

instance Functor LangF where
   fmap f (InitWorld next) = InitWorld (f . next)
   fmap f (ApplySystem w system next) = ApplySystem w system (f . next)
   fmap f (ForkSystem w system next) = ForkSystem w system (f . next)

type LangL = F LangF

initWorld :: LangL World
initWorld = liftF $ InitWorld id

applySystem :: World -> (GameHandle -> System' a) -> LangL a
applySystem w system = liftF $ ApplySystem w system id

forkSystem :: World -> (GameHandle -> System' a) -> LangL a
forkSystem w system = liftF $ ForkSystem w system id