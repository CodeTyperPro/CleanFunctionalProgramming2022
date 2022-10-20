module HW4
import StdEnv

// Write your Neptun code here -  HEIOPO            

/* Two lists are given. The first list contains instructive elements of what to do with the second list
1st list of char : [x,y,z] -> x => how many times to implement y function
					 		  y => the function / possible values '+' '*' '/'
					 		  z => value to use in the function
					 		  
For example if the first list is ['2', '+', '4'], it means add 4 to each element of the second list
2 times. So if the second list is [3, 10, 35] the result will be 
[3 + 4 + 4, 10 + 4 + 4, 35 + 4 + 4] = [11, 18, 43]

Try to use higher order function if possible. */

// Convert Char to Int
ToInt :: Char -> Int
ToInt c = toInt(c - '0');

// Real computation
ExecuteOperation :: [Char] Int -> Int
ExecuteOperation [x, '+', z] n = n + ToInt(x)*(ToInt(z))
ExecuteOperation [x, '*', z] n = n*(ToInt(z)^(ToInt(x)))
ExecuteOperation [x, '/', z] n
| z == '0' && ToInt x > 0 = abort "Sorry, division by 0 is undefined behaviour."
= n/(ToInt(z)^ToInt(x))

// Used for validation 2nd level
checkInput :: [Char] Int -> Int
checkInput [x, y, z] n
| ((x >= '0' &&  x<='9') && (z>='0' && z <= '9') && (any ((==) y)['+','*', '/'])) = ExecuteOperation [x, y, z] n
= abort "Please, insert a valid input."

// Used for validade 1st level
checkSize :: [Char] Int -> Int
checkSize [] n = abort "We can't execute because the given list must have exactly 3 elements."
checkSize x n
| y<3 || y>3 = abort "Please, insert a valid input. The given list must have exactly 3 valid elements."
= checkInput x n 
where y = length x

// All functions above are just for validation

cond :: [Char] Int -> Int
cond x n = checkSize x n

calc :: [Char] [Int] -> [Int]
calc op x = map (cond op) x

//Start = calc ['2', '+', '4'] [3, 10, 35] // [11, 18, 43]
//Start = calc ['0', '*', '1'] [24, 35, 56] //= [24,35,56]

// My input test cases :)
//Start = calc ['2', '/', '4'] [16] // 1
//Start = calc [] [12, 4] // Invalid input message
//Start = calc ['a', '+', 'w'] [12] // Invalid input message
//Start = calc ['a', '-', '*'] [12] //  Invalid input message
//Start = calc ['8', '-', '?'] [12] //  Invalid input message
//Start = calc ['5', '-', '0'] [12] //  Invalid input message
//Start = calc ['5', '+', '0'] [12] //  Error message
//Start = calc ['5', '/', '0'] [12] //  Division by 0 message
//Start = calc ['5', '/', '8', '9'] [12]  //Invalid input message
//Start = calc ['-', '-', '-'] [1, 2, 3, 4] //Invalid input message
//Start = calc ['1', '2'] [1, 2, 3, 4]  Error message
//Start = calc ['2', '/', '4'] [25]  // 1
//Start = calc ['0', '/', '0'] [12, 32, 43]  // [12, 32, 43] 
 
/* In two dimensional list, you are given balances of clients of a bank. Each list 
represents the activities of one client. The first number is the starting balance of this month, 
and the following numbers are the deposits(+N) and the withdrawals(-N). Calculate their 
closing balance, and depending on the amount calculate their interest and add it and return in single list

Criterias of interest calculation:
if balance <= 30'000 : interest = 1%
if balance > 30'000 and balance <= 100'000  : interest = 5%
if balance > 100'000 and balance <= 200'000  : interest = 8%
if balance > 200'000 : interest = 10% (You can round up in case interest is a fractional number)


Example: interest [[50000, 900, -4000, 80000], [900, -800, 9000, 5000]] => (closing balance) [[126900], [14100]] => (interest added) [137052, 14241] */

compute :: [Int] Int Int -> Int
compute [] inflow outflow = inflow - outflow
compute [x: xs] inflow outflow
| x < 0 = compute xs inflow (outflow + (~x))
= compute xs (inflow + x) outflow

calculateInterest :: Int -> Int
calculateInterest x
| x <= 30000 = x + toInt(toReal(x)*0.01)
| x > 30000 && x <= 100000 = x + toInt(toReal(x)*0.05)
| x > 100000 && x <= 200000 = x + toInt(toReal(x)*0.08)
= x + toInt(toReal(x)*0.1)

interestAux :: [[Int]] -> [Int]
interestAux [] = []
interestAux [x:xs] = [(hd x + compute (tl x) 0 0)] ++ interestAux xs

interest :: [[Int]] -> [Int]
interest [] = []
interest x = map calculateInterest (interestAux x)

//Start = interest [[0]] // [[0]]
//Start = interest [[200000, -9000, 45000, -4578], [100000, 7895, -6782], [45936, -3792, 7849, 3739], [3543, 8953, -4932]] // [254564, 109202, 56419, 7640]

// Test case added
//Start = interest [[50000, 900, -4000, 80000], [900, -800, 9000, 5000]] // [137052, 14241]
//Start = interest []