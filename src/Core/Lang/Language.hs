-- | Script monad to manipulate Game by applying Systems
{-# LANGUAGE GADTs #-}
-- | TO DO
-- Add thread forking
-- Pull down ECS language
-- Suggestion: hide explicit w tossing, behind the script, like with GameHandle.
module Core.Lang.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Game.Handle (GameHandle)
import World (World)

import qualified Core.ResourceManager.Language as L
import qualified Core.SystemControl.Language as L

data LangF next where
   InitWorld :: (World -> next) -> LangF next
   -- ^ Inits World record with component maps.
   RunECSystems :: World 
                -- ^ Starting World state.
                -> L.SystemControlL a
                -- ^ Script that defines one step of a loop.
                -> (a -> next)
                -> LangF next
   -- ^ Runs system control script.
   EvalResourceManager :: (GameHandle -> L.ResourceManagerL a) -> (a -> next) -> LangF next
   -- ^ Evals ResourceManagerL,
   -- specific language for resource loading/cashing/unloading scripts.
   
instance Functor LangF where
   fmap f (InitWorld next) = InitWorld (f . next)
   fmap f (RunECSystems w systemControlScript next) = RunECSystems w systemControlScript (f . next)
   fmap f (EvalResourceManager rmScript next) = EvalResourceManager rmScript (f . next)
   
type LangL = F LangF

initWorld :: LangL World
initWorld = liftF $ InitWorld id

runECSystems :: World -> L.SystemControlL a -> LangL a
runECSystems w systemControlScript = liftF $ RunECSystems w systemControlScript id

evalResourceManager :: (GameHandle -> L.ResourceManagerL a) -> LangL a
evalResourceManager rmScript = liftF $ EvalResourceManager rmScript id