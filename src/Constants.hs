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

-- | 0 element is Maximum eps.
epsHistoryLength :: Int
epsHistoryLength = 100 + 1

-- | In pixels.
pulseHeight :: Int
pulseHeight = 50

-- >>