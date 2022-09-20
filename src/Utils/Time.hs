-- | Utility functions related to filepath, folders, files, etc.
module Utils.Time
   ( fpsToTics
   ) where

import Types.Time (FPS, HiResTime)

fpsToTics :: FPS -> HiResTime
fpsToTics fps = (\n -> if n == 0 then 1 else n)
   . floor $ 1000/(fromIntegral fps :: Double)