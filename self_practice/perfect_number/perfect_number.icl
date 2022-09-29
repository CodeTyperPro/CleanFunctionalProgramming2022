module perfect_number
import StdEnv

/*
1,	Given an integer, x, calculate the total number of perfect numbers between 0 to that number, [1,x).
	Perfect number is a number which is equal to the sum of its divisor.
	For example: 6 is a perfect number
				divisor of 6 = 1, 2, 3 => sum of its divisor = 6 == 6
				10 is not a perfect number
				divisor of 10 = 1, 2, 5 => sum = 8 != 10
				
	If the given x is 10, then the result is 1 because there is only 1 perfect number between 1 and x.
	
	You are advised to write one or more functions for clear coding. 
*/

sumOfDivisor :: Int Int-> Int
sumOfDivisor x i
| i == x = 0
| (x rem i) == 0 = i + sumOfDivisor x (i + 1)
= sumOfDivisor x (i+1)

numOfPerfectNumber :: Int -> Int
numOfPerfectNumber x
| x <= 0 = 0
| (sumOfDivisor x 1) == x = 1 + numOfPerfectNumber (x-1)
= numOfPerfectNumber (x-1)

//Start = numOfPerfectNumber 29 	// 2
//Start = numOfPerfectNumber 1000 	// 3
//Start = numOfPerfectNumber 0 		// 0
Start = numOfPerfectNumber 1 		// 0
//Start = numOfPerfectNumber 10000 	// 4