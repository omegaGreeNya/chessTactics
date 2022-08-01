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
import Apecs.TH

import World.Components

makeWorld "World" [''CPosition, ''CVelocity, ''CRenderable]

type System' a = System World a