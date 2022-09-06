module Types.Game.Handle
   ( GameHandle (..)
   ) where

import Types.Game.SubSystems (SubSystemsHandle)
import Types.App (AppCaches)

-- | GameHandle contains all handles, all global mutables.
data GameHandle = GameHandle
   { hSystems :: SubSystemsHandle
   , hCaches :: AppCaches
   }