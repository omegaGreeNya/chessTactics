-- |
module Game.SystemsStack
   ( createDrawStack
   , createStepStack
   , createUserInputStack
   ) where

import Systems (userInputSystem, stepSystem, drawSystem)
import Types.Time (HiResTime)
import Types.SystemsStack (SystemsStack)

import Language

createStepStack :: SystemControlL (SystemsStack HiResTime)
createStepStack = createSystemsStack [stepSystem]
   
createDrawStack :: SystemControlL (SystemsStack Double)
createDrawStack = createSystemsStack [drawSystem]

createUserInputStack :: SystemControlL (SystemsStack HiResTime)
createUserInputStack = createSystemsStack [userInputSystem]