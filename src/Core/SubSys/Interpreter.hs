module Core.SubSys.Interpreter where

import Control.Monad.Free.Church (foldF)
import qualified Apecs (runWith)

import Types.Game.Handle (GameHandle)
import World (World)

import qualified Core.SubSys.Language as L


interpretSubSysF :: GameHandle -> World -> systemTime -> L.SubSysF systemTime a -> IO a
interpretSubSysF h w time (L.CreateSystemsStack sysList next) = do
   let f system = Apecs.runWith w $ system h time
   mapM_ f sysList
   return $ next ()

runSystemStack :: GameHandle -> World -> systemTime -> L.SubSysL systemTime a -> IO a
runSystemStack h w time = foldF $ interpretSubSysF h w time