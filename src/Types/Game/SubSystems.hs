-- | Module difines subsystems handle.
-- This is needed due complexity of GameHandle.
-- Parts of GameHandle depends on other parts of it.
-- So, to breake dead-loop, there is that level of module granularity.
--
-- For example, consider this loop GameHandle -> AppCache -> TextureCache -> Cache.Class -> Resource.Class -> GameHandle
-- Now, it's will look like this GameHandle -> AppCache -> TextureCache -> Cache.Class -> Resource.Class -> SubSystems
module Types.Game.SubSystems
   ( SubSystemsHandle (..)
   , SDLHandle (..)
   ) where

import qualified SDL

-- | GOD-SubSys handle record.
data SubSystemsHandle = SubSystemsHandle
   { hSDL :: SDLHandle
   }

-- | SDL handles.
data SDLHandle = SDLHandle
   { hWindow   :: SDL.Window
   , hRenderer :: SDL.Renderer
   }