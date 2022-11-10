implementation module Q

import StdEnv

// definition of the Q type

::	Q  = { nom :: Int
         , den :: Int
         }  

simplify :: Q -> Q
simplify {nom=n,den=d}
| d == 0 = abort " denominator is 0"
| d < 0 = { nom = ~n/g, den = ~d/g}
| otherwise = { nom = n/g, den = d/g}
where g = gcd n d

mkQ :: Int Int -> Q
mkQ n d = simplify { nom = n, den = d }

equalQ :: Q Q -> Bool
equalQ x y = x.nom*y.den == y.nom*x.den

smallerQ :: Q Q -> Bool
smallerQ x y = x.nom*y.den < y.nom*x.den

plusQ :: Q Q -> Q
plusQ x y = mkQ (x.nom*y.den+y.nom*x.den) (x.den*y.den)

decrementQ :: Q Q -> Q
decrementQ x y = mkQ (x.nom*y.den - y.nom*x.den) (x.den*y.den)

timesQ :: Q Q -> Q
timesQ x y = mkQ (x.nom*y.nom) (x.den*y.den)

divideQ :: Q Q -> Q
divideQ x y = mkQ (x.nom*y.den) (x.den*y.nom)

absoluteQ :: Q -> Q
absoluteQ x = mkQ (abs x.nom) (abs x.den)

signOfQ :: Q -> Int
signOfQ x 
| y > 0.0 = 1
| y == 0.0 = 0
| y < 0.0 = -1
where y = toReal x.nom / toReal x.den

negateQ :: Q -> Q
negateQ x = mkQ (~x.nom) x.den

IntToQ :: Int -> Q
IntToQ x = mkQ x 1

QtoInt :: Q -> Int
QtoInt x = x.nom / x.den

isIntQ :: Q -> Bool
isIntQ x = x.den == 1 

QtoReal :: Q -> Real
QtoReal x = toReal x.nom / toReal x.den

QZero = { nom = 0, den = 1 }
QOne = { nom = 1, den = 1 }
q2 = { nom = 1, den = 2 }
q3 = { nom = 3, den = 4 }

//Start = simplify (mkQ 81 90) // (Q 9 10)
//Start = mkQ 81 90 // (Q 9 10)
//Start = equalQ (mkQ 9 10) (mkQ 81 90) // True
//Start = smallerQ q2 q3 // True
//Start = plusQ q2 QZero // (Q 1 2)
//Start = decrementQ QZero q3 // (Q -3 4)
//Start = timesQ QOne q2 // (Q 1 2)
//Start = divideQ QOne q2 // (Q 2 1)
//Start = absoluteQ QZero // (Q 0 1)
//Start = signOfQ q2 // 1
//Start = negateQ q2 // (Q -1 2)
//Start = IntToQ 4 // (Q 4 1)
//Start = QtoInt q2 // 0
//Start = isIntQ QOne // True
Start = QtoReal q2 // 0.5
