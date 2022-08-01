-- | Main script monad to start up/run/shutdown game.
{-# LANGUAGE GADTs #-}
module Core.Game.Language where

import Control.Monad.Free.Church (F(..), liftF)

import Types.Game.Config (GameConfig)
import Types.Game.Handle (GameHandle)

import qualified Core.Lang.Language as L

data GameF next where
   InitGame :: GameConfig -> (GameHandle -> next) -> GameF next
   -- ^ Inits external systems and returns main-handle record.
   EvalLang :: GameHandle -> L.LangL a -> (a -> next) -> GameF next
   -- ^ Evals lang script (basicly runs game loop)
   ShutDown :: GameHandle -> (() -> next) -> GameF next
   -- ^ Shut down all external systems.

instance Functor GameF where
   fmap f (InitGame cfg next) = InitGame cfg (f . next)
   fmap f (EvalLang h runner next) = EvalLang h runner (f . next)
   fmap f (ShutDown h next) = ShutDown h (f . next)
   

type GameL = F GameF

initGame :: GameConfig -> GameL GameHandle
initGame cfg = liftF $ InitGame cfg id

run :: GameHandle -> L.LangL a -> GameL a
run h runner = liftF $ EvalLang h runner id

shutDown :: GameHandle -> GameL ()
shutDown h = liftF $ ShutDown h id