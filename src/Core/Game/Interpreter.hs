-- | GameL script interpreters defined here
module Core.Game.Interpreter where

import Control.Monad.Free.Church (foldF)

import Initialization (initGame, shutdownGame)

import Core.Lang.Interpreter (langInterpret)
import qualified Core.Game.Language as L

gameInterpretF :: L.GameF a -> IO a
gameInterpretF (L.InitGame cfg next) = do
   gameHandle <- initGame cfg
   return $ next gameHandle
gameInterpretF (L.EvalLang h runner next) = do
   result <- langInterpret h runner
   return $ next result
gameInterpretF (L.ShutDown h next) = do
   shutdownGame h
   return $ next ()

launchGame :: L.GameL a -> IO a
launchGame = foldF $ gameInterpretF