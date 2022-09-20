-- |
module Game.SystemsStack
   ( drawStack
   , stepStack
   , userInputStack
   ) where

import Systems (userInputSystem, stepSystem, drawSystem)
import Types.Time (HiResTime)

import Language

stepStack :: SubSysL HiResTime ()
stepStack = createSystemsStack [stepSystem]
   
drawStack :: SubSysL Double ()
drawStack = createSystemsStack [drawSystem]

userInputStack :: SubSysL HiResTime ()
userInputStack = createSystemsStack [userInputSystem]