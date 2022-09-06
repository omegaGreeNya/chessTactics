-- | LangL script interpreters defined here

-- TO DO
-- IMPLEMENT FORKING
module Core.Lang.Interpreter 
   (langInterpret) where

import qualified Apecs (runWith)
import Control.Monad.Free.Church (foldF)

import Types.Game.Handle (GameHandle)
import World (initWorld)

import Core.ResourceManager.Interpreter (runResourceManager)
import qualified Core.Lang.Language as L

langInterpretF :: GameHandle -> L.LangF a -> IO a
langInterpretF _ (L.InitWorld next) = do
   w <- initWorld
   return $ next w
langInterpretF h (L.ApplySystem w system next) = do
   result <- Apecs.runWith w (system h)
   return $ next result
langInterpretF h (L.EvalResourceManager rmScript next) = do
   result <- runResourceManager h (rmScript h)
   return $ next result

langInterpret :: GameHandle -> L.LangL a -> IO a
langInterpret h = foldF $ langInterpretF h