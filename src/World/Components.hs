--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module World.Components where

import Linear
import qualified Apecs

import Types (Picture)
import Stores.Cache.Texture (TextureCache)

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

-- << Caches
newtype CTextureCache = CTextureCache TextureCache

-- | DUMMY INSTANCES FOR APECS MACHINERY DO NOT USE 
-- DO NOT USE DO NOT USE DO NOT USE DO NOT USE DO NOT USE DO NOT USE 
instance Semigroup CTextureCache where
   (<>) = const

-- | DUMMY INSTANCES FOR APECS MACHINERY DO NOT USE 
-- DO NOT USE DO NOT USE DO NOT USE DO NOT USE DO NOT USE DO NOT USE 
instance Monoid CTextureCache where
   mempty = error "CTextureCache was used without setting it's value"
-- Very sorry about error, but this is Apecs fault.. =)

instance Apecs.Component CTextureCache where
   type Storage CTextureCache = Apecs.Global CTextureCache
-- >>