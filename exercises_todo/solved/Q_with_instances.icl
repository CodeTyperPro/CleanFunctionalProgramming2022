implementation module Q_with_instances

import StdEnv


// definition of the Q type


::	Q  = { nom :: Int
         , den :: Int
         }  


QZero = { nom = 0, den = 1 }
QOne = { nom = 1, den = 1 }
q2 = { nom = 1, den = 2 }
q3 = { nom = 3, den = 4 }
q4 = { nom = -1, den = 4 }


//// Instances for Q type


simplify :: Q -> Q
simplify {nom=n,den=d}
| d == 0 = abort " denominator is 0"
| d < 0 = { nom = ~n/g, den = ~d/g}
| otherwise = { nom = n/g, den = d/g}
   where g = gcd n d


mkQ :: Int Int -> Q
mkQ n d = simplify { nom = n, den = d }



instance == Q
where 
    (==) x y = x.nom*y.den == y.nom*x.den 


Start = (mkQ 9 10) == (mkQ 81 90) // True



instance < Q
where 
    (<) x y = x.nom*y.den < y.nom*x.den
    
//Start = mkQ 1 2 < mkQ 3 4  // True



instance + Q
where 
    (+) x y = mkQ (x.nom*y.den+y.nom*x.den) (x.den*y.den)

//Start = mkQ 2 4 + mkQ 5 6 // (Q 4 3)



instance - Q
where 
    (-) x y = mkQ (x.nom*y.den-y.nom*x.den) (x.den*y.den)

//Start = mkQ 2 4 - mkQ 5 6 // (Q -1 3)



instance zero Q
where 
    zero = fromInt 0 

//Start :: Q   
//Start = zero // (Q 0 1)
//Start = (q2 + zero) == q2 // True



instance * Q
where 
    (*) x y = mkQ (x.nom*y.nom) (x.den*y.den)

//Start = QOne * q2 // (Q 1 2)



instance / Q
where 
    (/) x y = mkQ (x.nom*y.den) (x.den*y.nom)

//Start = QOne / q2 // (Q 2 1)



instance one Q
where 
    one = fromInt 1 // 

//Start :: Q   
//Start = one // (Q 1 1)
//Start = (q3 * one) == q3 // True



instance abs Q
where
     abs x = mkQ (abs x.nom) (abs x.den)

//Start = abs q3 // (Q 3 4)



instance sign Q
where
     sign x
     | y > 0.0 = 1
     | y == 0.0 = 0
     | y < 0.0 = -1
     where y = toReal x.nom / toReal x.den
     
//Start = sign q2  // 1
//Start = sign q4  // -1



instance ~ Q
where 
    ~x = mkQ (~x.nom) x.den  // (Q 1 4)
    
//Start = ~q4



instance fromInt Q
where 
    fromInt i = mkQ i 1
    
//Start :: Q
//Start = fromInt 3 // (Q 3 1)



instance toInt Q
where 
    toInt x = x.nom / x.den
    
//Start = toInt QOne // 1
//Start = toInt q3 // 0



isIntQ :: Q -> Bool
isIntQ x = x.den == 1 

//Start = isIntQ QOne // True
//Start = isIntQ q2 // False



instance toReal Q
where 
    toReal x = toReal x.nom / toReal x.den

//Start = toReal q2 // 0.5
//Start = toReal q3 // 0.75



instance toString Q
where
    toString q
        | xq.den == 1 = toString xq.nom
        | otherwise = toString xq.nom +++"/"+++ toString xq.den
    where xq = simplify q
        
//Start = toString q3 // "3/4"



list_of_Q_strings :: [String]
list_of_Q_strings = [toString q \\ q <- [zero, mkQ 1 3 .. mkQ 3 2]]

//Start = list_of_Q_strings// ["0","1/3","2/3","1","4/3"]



//overloading can not be solved
//Start = toString zero+zero

/*
Start :: String
Start = toString sum  // "0"
where sum :: Q
      sum = zero + zero */


