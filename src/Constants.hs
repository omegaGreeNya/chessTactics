-- | Module defines different constants.
module Constants
   ( textureFolder
   , epsHistoryLength
   , pulseHeight
   ) where


-- <<< FilePaths

-- | Path to folder fith all textures.
textureFolder :: FilePath
textureFolder = ".\\data\\Images\\"

-- >>>

-- << Metrics

-- | How many eps for StackMetrics stored in history
epsHistoryLength :: Int
epsHistoryLength = 100

-- | In pixels.
pulseHeight :: Int
pulseHeight = 50

-- >>