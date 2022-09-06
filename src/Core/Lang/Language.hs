-- | Script monad to manipulate Game by applying Systems
{-# LANGUAGE GADTs #-}
-- | TO DO
-- Add thread forking
-- Pull down ECS language
module Core.Lang.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Game.Handle (GameHandle)
import World (World, System')

import qualified Core.ResourceManager.Language as L

data LangF next where
   InitWorld :: (World -> next) -> LangF next
   -- ^ Inits World record with component maps.
   ApplySystem :: World -> (GameHandle -> System' a) -> (a -> next) -> LangF next
   -- ^ Applies system to world.
   --ForkSystem :: World -> (GameHandle -> System' a) -> (a -> next) -> LangF next
   -- ^ Applies system in different thread 
   -- useful for launching many loop-systems
   EvalResourceManager :: (GameHandle -> L.ResourceManagerL a) -> (a -> next) -> LangF next
   -- ^ Evals ResourceManagerL,
   -- specific language for resource loading/cashing/unloading scripts.
   
instance Functor LangF where
   fmap f (InitWorld next) = InitWorld (f . next)
   fmap f (ApplySystem w system next) = ApplySystem w system (f . next)
--   fmap f (ForkSystem w system next) = ForkSystem w system (f . next)
   fmap f (EvalResourceManager rmScript next) = EvalResourceManager rmScript (f . next)
   
type LangL = F LangF

initWorld :: LangL World
initWorld = liftF $ InitWorld id

applySystem :: World -> (GameHandle -> System' a) -> LangL a
applySystem w system = liftF $ ApplySystem w system id

{-
forkSystem :: World -> (GameHandle -> System' a) -> LangL a
forkSystem w system = liftF $ ForkSystem w system id
-}

evalResourceManager :: (GameHandle -> L.ResourceManagerL a) -> LangL a
evalResourceManager rmScript = liftF $ EvalResourceManager rmScript id