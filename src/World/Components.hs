{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module World.Components where

import Linear
import qualified Apecs

import Types (Picture)

-- | Entity 2-d position on map (not on screen)
data CPosition = CPosition
   { position :: V2 Double
   } deriving Show

instance Apecs.Component CPosition where
   type Storage CPosition = Apecs.Map CPosition

-- | Velocity of entity
data CVelocity = CVelocity
   { velocity :: V2 Double
   } deriving Show

instance Apecs.Component CVelocity where
   type Storage CVelocity = Apecs.Map CVelocity


-- | Data to render entity on streen
data CRenderable = CRenderable
   { picture :: Picture
   }

instance Apecs.Component CRenderable where
   type Storage CRenderable = Apecs.Map CRenderable
