module ex4_todo

import StdEnv

/*
	NEPTUNE: HEIOPO
	NAME: MARTINS ALFREDO
*/

// 29. (bonus point) rewrite map using foldr
mymap :: (a -> b) [a] -> [b]
mymap function x = foldr ((\x y = [ function x : y ] )) [] x

//Start = mymap inc [1..10]
    
// 30. (bonus point) Compute the average of a list of float point numbers using the foldr function
// in one line code using one lambda function.
avg :: [Real] -> Real
avg x = (foldr ((\x y = x+y)) 0.0 x)/toReal((length(x)))

//Start = avg [16.2, 17.8, 11.5] // 15.1666666666667
//Start = avg [13.0, 40.9] // 26.95


// 31. (bonus point) Write a function that takes a list of numbers and adds the first element,
// subtracts the second element, adds the third element, subtracts the fourth element, so on, 
// in this alternating repetition.
// For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3

alter :: (Int, Int) -> Int
alter (x,y)
| isOdd y = x
= ~x

alternatingSum :: [Int] -> Int
alternatingSum x = sum (map alter (zip (x, [1..n])))
where n = (length x)


//Start = alternatingSum [2..7] //-3
//Start = alternatingSum [45,-5,63,46,-345,4321] //-4599
Start = alternatingSum [] //0
