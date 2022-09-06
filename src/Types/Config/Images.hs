module Types.Config.Images where
{-

-- | Module defines Image config structure and json parsing methods
-- TO-DO: Make dynamic constructor, 
--    based on files inside "./data/Images/" directory.
module Types.Config.Images
   (ImageCfg(..)) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data ImagesCfg = ImagesCfg
   { imgPaths :: [FilePath]
   } deriving (Show, Generic)

deriveJSON defaultOptions ''ImageCfg

{- JSON object example

{ 
   "ImagesCfg": {
      "imagesPaths": [
      "./diblook.png",
      "./Cow/cow_pink.png"
      ]
   }
}

-}

-}