module HW3
import StdEnv

/* Please enter your name and neptun code here 
	Name: MARTINS Alfredo
	Neptune-code: HEIOPO
*/

// Try to use higher order functions if you can

/* 1. Given a list of numbers return the index of the number that including that
number the sum of all previous numbers equals 0.

Example:
equalsZero [1,2,3,-3,-2,-1,3,4,1,-1] = 5
which is the index of -1. The sum of [1,2,3,-3,-2,-1] is zero.
If there is no such number return -1 */

plus x y = x + y

incWhileSum :: (Int -> Bool) [Int] -> Int
incWhileSum p [] = -1
incWhileSum p [0] = 0
incWhileSum p [_] = -1
incWhileSum p [x, y:xs]
| p x = 0
| not (p x) =  1 + incWhileSum p [plus x y : xs]
= -1

equalsZero :: [Int] -> Int
equalsZero x = incWhileSum ((==) 0) x

//Start = equalsZero [1,2,3,-3,-2,-1,3,4,1,-1] // 5
//Start = equalsZero [6,-6,0,0] // 1
//Start = equalsZero [1, 2, 3, 4] // -1
//Start = equalsZero [] // -1
//Start = equalsZero [0]
//Start = equalsZero [-1] // -1
//Start = equalsZero [1,2,3,-3,-1,-5,-2,5] // 7 


/* 2. Accountants are always interested in finding numbers that contain the digit 7. 
Implement a function that filters if the number does not contain digit 7
Example: 
hasSeven [3, 13, 27, 771, 674, 301] = [27, 771, 674] */

areEqual x y = abs(x) == abs(y) // abs to handle negative numbers

containsDigit :: (Int -> Bool) Int -> Bool
containsDigit p 0 = False
containsDigit p  x
| p d = True
| not (p d) = containsDigit p (x/10)
= False
where d = x rem 10

hasSeven :: [Int] -> [Int]
hasSeven x = filter (containsDigit (areEqual 7)) x

//Start = hasSeven [3, 13, 27, 771, 674, 301] // [27, 771, 674]
//Start = hasSeven [3, 5 , 6, -437] // [-437]
//Start = hasSeven [-7, -17, -437, -47, 54] // [-7, -17, -437, -47]