module ex1

import StdEnv

//Exercises 
// 1. Define a function maxi with two arguments that delivers the maximum of the two.
maxi :: Int Int -> Int
maxi x y 
| x > y = x
= y

//Start = maxi 34 56 // 56



// 2. Define a function mini that has two arguments that delivers the minimum of the two.
mini :: Int Int -> Int
mini x y 
| x < y = x
| x == y = y     // = y
| x > y = y 

//Start = mini 34 56  // 34



// 3. Triple a number.
triple :: Int -> Int
triple x = 3*x

//Start = triple 5 // 15



// 4. Check if a number is odd.
isoddnr :: Int -> Bool  // isEven, isOdd built-in functiona
isoddnr x = ((x rem 2) == 1)

//Start = isoddnr 6 // False
//Start = isoddnr 21 // True



// 5. Check if a number is the sum of two other given numbers in any order.
issum :: Int Int Int -> Bool
issum a b c = (a == b + c) || (b == a + c) || (c == a+b)

//Start = issum 10 6 3  // False
//Start = issum 10 6 4  // True



// 6. Add 100 to a number.
add100 :: Int -> Int
add100 x = x + 100

//Start = add100 5  // 105



// 7. Check if a number is multiple of 10.
ismult10 :: Int -> Bool
ismult10 x = x rem 10 == 0

//Start = ismult10 20 // True
//Start = ismult10 201 // False

// long version
ismult102 :: Int -> Bool
ismult102 x 
| x rem 10 == 0 = True
| otherwise = False

//Start = ismult102 20 // True
//Start = ismult102 202  // False



// 8. Say Hello to the World!
st :: String String String -> String
st a b c = a +++ b +++ c

//Start = st "Hello" " World!" " from FP class"



// 9. Change a character if is small letter to uppercase and vice versa,
// if is a special sign, leave as it is.
ch :: Char -> Char
ch c
| 65 <= (toInt c) && (toInt c) <= 90 = toLower c
| 97 <= (toInt c) && (toInt c) <= 122 = toUpper c
= c
 
//Start = (ch 'a', ch 'z', ch 'A', ch 'Z', ch 'x', ch '%') // ('A','Z','a','z','X','%')



// 10. Write a function to compute the absolute value of an integer number.

//Start = abs -4   // 4  abs built-in function

myabs :: Int -> Int
myabs x
| x < 0 = ~x  //unary - with one operator is ~ tilde
| x == 0 = 0
| x > 0 = x

//Start = myabs -4 // 4
//Start = myabs 8 // 8



// 11. Given an integer, write a function that returns the last digit.

lastDigit :: Int -> Int
lastDigit nr = nr rem 10

//Start = lastDigit 124 // 4
//Start = lastDigit 4000 // 0
//Start = lastDigit 123442 // 2



// 12. Given three integer numbers a, b and c. 
// Check if both a and b have the same remainder when divided by c.
sameRem :: Int Int Int -> Bool
sameRem a b c = (a rem c) == (b rem c)

//Start = sameRem 12 4 4 // True
//Start = sameRem 12 4 3 // False
//Start = sameRem 13 4 3 // False



// 13. Given two integers and a boolean value. 
// Check if the first integer is even, the second divisible by 13 
// and the boolean value is True

check :: Int Int Bool -> Bool
check a b boolean = (isEven a) && (b rem 13 == 0) && boolean

// long version
//| (isEven a) && (b rem 13 == 0) && boolean == True  
//| otherwise  = False

//Start = check 4 26 True // True
//Start = check 5 26 True // False
//Start = check 5 23 True // False
//Start = check 4 26 False // False
//Start = check 6 26 False // False



// 14. Add the numbers from 1..N in a recursive function, where N is positive.
addn :: Int -> Int
addn n
| n <= 0 = abort " N can not be zero or negative"
| n == 1 = 1 
= n + addn (n-1)

//Start = addn 5
// 5 + addn 4
// 5 + 4 + addn 3
// 5 + 4 + 3 + addn 2
// 5 + 4 + 3 + 2 + addn 1 
// 5 + 4 + 3 + 2 + 1
// 15
//Start = addn -10 // N can not be zero or negative
//Start = addn 0 // N can not be zero or negative



// 15. Compute the cube of a number.
cube :: Int -> Int
cube x = x^3 // ^ caret - power operator

//Start = cube 4  // 64
//Start = cube 8  // 512

cube2 :: Int -> Int
cube2 x = x*x*x

//Start = cube2 4 // 64



// 16. Add the digits of a number e.g. for 123 is 6.
sumDigits :: Int -> Int
sumDigits x 
| x < 10 = x 
= x rem 10 + sumDigits (x/10) 

//Start = sumDigits 12345 // 15
// 5 + sumDigits 12345/10  =  5 + sumDigits 1234
// 5 + 4 + sumDigits 1234/10 =  5 + 4 + sumDigits 123
// 5 + 4 + 3 + sumDigits 123/10 =  5 + 4 + 3 + sumDigits 12 
// 5 + 4 + 3 + 2 + sumDigits 12/10 =  5 + 4 + 3 + 2 + sumDigits 1 
// 5 + 4 + 3 + 2 + 1
// 15

//Start = sumDigits 5 // 5
//Start = sumDigits -54 // 9



// 17. Check if a number is divisible by 9! (using sumDigits)
// a number is divisible by 9 is the sum of digits is divisible by 9
div9 :: Int -> Bool
div9 x = (sumDigits x) rem 9 == 0

//Start = div9 81 // True
//Start = div9 800 // False



// 18. Write a function that takes two arguments, say n and x, and computes their power,
// in 2 versions - with recursion and without recursion.
power :: Int Int -> Int
power x y = x^y 

//Start = power 2 5 // 32

powerrec  :: Int Int -> Int
powerrec x n
| n == 0 = 1
= x * powerrec x (n-1)

//Start = powerrec 2 0 // 1
//Start = powerrec 2 4 // 16
// powerrec 2 4
// 2 * powerrec 2 3
// 2 * 2 * powerrec 2 2
// 2 * 2 * 2 * powerrec 2 1
// 2 * 2 * 2 * 2 * powerrec 2 0
// 2 * 2 * 2 * 2 * 1
// 16



// 19. Use 18. to write a function that squares its argument.
sq :: Int -> Int
sq x = power x 2

//Start = sq 8 // 64
//Start = sq 0 // 0



// 20. Compute factorial n recursively, where n! = n*(n-1)! .
factor :: Int -> Int
factor n
| n == 0 = 1
| n > 0 = n * factor (n-1)

//Start = factor 5 // 120
// factor 5
// 5 * factor 4
// 5 * 4 * factor 3
// 5 * 4 * 3 * factor 2
// 5 * 4 * 3 * 2 * factor 1
// 5 * 4 * 3 * 2 * 1 * factor 0
// 5 * 4 * 3 * 2 * 1 * 1
// 120



// 21. Given two integers, put their digits together like: 123 456 =123456
countDigits :: Int -> Int
countDigits x 
| x < 10 = 1 
= 1 + (countDigits (x/10)) 

//Start = countDigits 456 // 3

glue :: Int Int -> Int 
glue a b = a * (10^(countDigits b)) + b

//Start = glue 123000 456 // 123000456
//Start = glue 765 432 // 765432

glue2 :: Int Int -> Int
glue2 a b = toInt ((toString a) +++ (toString b)) 

//Start = glue2 123000 456 // 123000456
//Start = glue2 765 432 // 765432



// 22. Given a positive integer, find the sum of the odd numbers up to that number starting from 1.
sumOdd :: Int -> Int
sumOdd n 
| n < 1 = abort " n has to be positive"
| n == 1 = 1
| n > 1 && isOdd n = n + sumOdd (n-1)
| n > 1 && isEven n = sumOdd (n-1)

//Start = sumOdd 5 // 9 
//Start = sumOdd 21 // 121
//Start = sumOdd 10 // 25 = 9+7+5+3+1
//Start = sumOdd -13 // n has to be positive

sumOdd2 :: Int -> Int
sumOdd2 n 
| n < 1 = abort " n has to be positive"
| isEven n = sumOdd2 (n-1)
| n == 1 = 1
| n > 1 = n + sumOdd2 (n-2)

//Start = sumOdd2 5 // 9 
//Start = sumOdd2 21 // 121
//Start = sumOdd2 10 // 25
Start = sumOdd2 -13 // n has to be positive
 