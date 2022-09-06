{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}


module World
   ( World(..)
   , System'
   , initWorld
   ) where

import Apecs

import World.Components

makeWorld "World" [''CTextureCache, ''CPosition, ''CVelocity, ''CRenderable]

type System' a = System World a