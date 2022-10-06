module HW1
import StdEnv

//Please write your neptun code here:
/*
Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.

    Hint : 
        1. The basis of functional programming is functions , if a problem can't be solved
        with one function divide it into multiple functions.
        2. To abort with a message you can use the 'abort' function.
        3. The logic needs to be thought by you , the implementation you can figure out 
        by going through lecture slides and practice material.
*/



/*
	100 = 10 + 10 + 10 + 10 + 10 + 
*/


//Define a function to find the minimum number of currency notes to be returned by an ATM machine for a given amount of money.
//The currency notes available in the ATM are 10, 5,2 and 1.
//The input is always a positive integer.

ATM :: Int -> Int
ATM x
| x >= 10 = 1 + ATM (x-10)
| x >= 5 = 1 + ATM (x-5)
| x >= 2 = 1 + ATM (x-2)
| x >= 1 = 1 + ATM (x-1)
= 0

//Start = ATM 100  // 10
//Start = ATM 99  // 12
//Start = ATM 28  // 5
//Start = ATM 1  // 1



/*

	gcd (a, b) = gcd(b, a%b) -> Euclidean Algorithm
	
      write a function to find the greatest common divisor of two numbers.
      If the gcd is one of the input numbers, then print "y is a multiple of x"
      otherwise print "neither number is a multiple of the other".
      If one of the input numbers is 0, then print "Cannot calculate gcd if  of 0" and abort.
      e.g: input: 6 18
           output: 6
           explanation: 6 is one of the input numbers, so print "either number is a multiple of the other"
      eg: input: 21 28
          output: 7
          explanation: 7 is a not any of the input numbers, so print "neither number is a multiple of the other"

          
*/
gcd :: Int Int -> Int
gcd a b 
| b == 0 = a
= gcd b (a rem b)

//Start = gcd 6 18 // 6

MyFunGCD :: Int Int -> Int
MyFunGCD x y
| x == 0 || y == 0 = abort "Cannot calculate gcd of 0"
| (m rem x == 0) || (m rem y == 0) = abort "either number is a multiple of the other"
| (m rem x <> 0) && (m rem y <> 0) = abort "neither number is a multiple of the other"
| (x rem y == 0) || (x rem y == 0) = abort "y is a multiple of x"
= m
where m = gcd x y

//Start = MyFunGCD 6 18 // "either number is a multiple of the other"
//Start = MyFunGCD 21 28 // "neither number is a multiple of the other"
//Start = MyFunGCD 0 0 // "Cannot calculate gcd of 0"