definition module Q

import StdEnv, StdDebug

::	Q 
 
mkQ :: Int Int -> Q

equalQ :: Q Q -> Bool

smallerQ :: Q Q -> Bool

plusQ :: Q Q -> Q

decrementQ :: Q Q -> Q

timesQ :: Q Q -> Q

divideQ :: Q Q -> Q

absoluteQ :: Q -> Q

signOfQ :: Q -> Int

negateQ :: Q -> Q

IntToQ :: Int -> Q

QtoInt :: Q -> Int

isIntQ :: Q -> Bool

QtoReal :: Q -> Real