-- | Script monad to manipulate Game by applying Systems
{-# LANGUAGE GADTs #-}
-- | TO DO
-- Add thread forking
-- Pull down ECS language
-- Suggestion: hide explicit w tossing, behind the script, like with GameHandle.
module Core.Lang.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Game.Handle (GameHandle)
import World (World, System')

import qualified Core.ResourceManager.Language as L
import qualified Core.SystemControl.Language as L

data LangF next where
   InitWorld :: (World -> next) -> LangF next
   -- ^ Inits World record with component maps.
   ApplySystem :: World -> (GameHandle -> System' a) -> (a -> next) -> LangF next
   -- ^ Applies system to world. Useful to apply init systems outside the loop.
   RunGameLoop :: World 
               -- ^ Starting World state.
               -> System' Bool
               -- ^ While this system applied on World returns True, execution continues.
               -> (loopState -> L.SystemControlL loopState)
               -- ^ Script that defines one step of a loop.
               -> loopState
               -- ^ Any loop metadata (lastCallTimes, skippedFrames, etc.)
               -> (() -> next)
               -> LangF next
   -- ^ Runs system in a loop. Basicly the thing that you can call main game loop.
   EvalResourceManager :: (GameHandle -> L.ResourceManagerL a) -> (a -> next) -> LangF next
   -- ^ Evals ResourceManagerL,
   -- specific language for resource loading/cashing/unloading scripts.
   
instance Functor LangF where
   fmap f (InitWorld next) = InitWorld (f . next)
   fmap f (ApplySystem w system next) = ApplySystem w system (f . next)
   fmap f (RunGameLoop w condition stepScript loopState next) = RunGameLoop w condition stepScript loopState (f . next)
   fmap f (EvalResourceManager rmScript next) = EvalResourceManager rmScript (f . next)
   
type LangL = F LangF

initWorld :: LangL World
initWorld = liftF $ InitWorld id

applySystem :: World -> (GameHandle -> System' a) -> LangL a
applySystem w system = liftF $ ApplySystem w system id

runGameLoop :: World -> System' Bool -> (loopState -> L.SystemControlL loopState) -> loopState -> LangL ()
runGameLoop w condition stepScript loopState = liftF $ RunGameLoop w condition stepScript loopState id

evalResourceManager :: (GameHandle -> L.ResourceManagerL a) -> LangL a
evalResourceManager rmScript = liftF $ EvalResourceManager rmScript id