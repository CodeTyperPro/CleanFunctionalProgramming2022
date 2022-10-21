module PT3
import StdEnv
import StdReal

/*
checkSumOfIntParts :: [[Real]] Int -> [Bool]
checkSumOfIntParts [] _  = [True]
checkSumOfIntParts [x : xs] n
| toInt(foldr (+) 0 x) <> n = [False]
= [True] ++ checkSumOfIntParts xs n

Start = checkSumOfIntParts [[1.2, 3.4, 5.6, 5.0], [1.2, 3.4]] 14 //

*/

Start = 1 + toReal 1.2