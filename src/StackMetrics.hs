{-# LANGUAGE RecordWildCards #-}
-- | Module defines StackMetrics,
-- Handle and methods to tack stacks execution timings.
module StackMetrics
   ( StackMetricsHandle
   , StackMetrics
   , StackId
   , initStackMetrics
   , getNewStackId
   , markStackExecution
   , drawMetrics
   ) where

import qualified SDL
import Linear
import Linear.Affine (Point(..))

import StackMetrics.Utils
         ( StackMetricsHandle
         , StackMetrics
         , StackPulse
         , StackId
         , initStackMetrics
         , getNewStackId
         , markStackExecution
         , getStackPulses
         , unwrapStackPulse
         )

import Constants (epsHistoryLength, pulseHeight)
import Types.Game.Handle (GameHandle(..))
import Types.Game.SubSystems (SubSystemsHandle(..), SDLHandle(..))
import Utils (setColor)


-- QUICK AND DIRTY

-- TO DO
-- Add id (fonts)
drawPulse :: SDLHandle -> StackId -> StackPulse -> IO ()
drawPulse SDLHandle{..} stackId stackPulse = do
   setColor hRenderer $ V4 0 0 0 10
   (maxEps, pulseList) <- unwrapStackPulse stackPulse
   let yOffset = fromIntegral $ (pulseHeight + 10) * (stackId - 1)
       fHeight x = fromIntegral . fromEnum $ (fromIntegral pulseHeight) * (x / maxEps)
       maxX = fromIntegral (epsHistoryLength - 1)
       
       draw n eps = 
         SDL.drawLine hRenderer (P (V2 n yOffset)) (P (V2 n (yOffset + fHeight eps)))
   mapM_ (uncurry draw) $ zip [maxX, maxX - 1..] pulseList
   {-
   putStrLn . show $ (\a -> (stackId, a)) $ zip [epsHistoryLength - 1, epsHistoryLength - 2..] pulseList
   putStrLn ""
   putStrLn ""
   putStrLn ""
   putStrLn ""
   -}
   

drawMetrics :: GameHandle -> IO ()
drawMetrics GameHandle{..} = do
   case hMetrics hSystems of
      Just h -> do
         pulseList <- getStackPulses h
         mapM_ (uncurry (drawPulse $ hSDL hSystems)) pulseList
      _        -> return ()