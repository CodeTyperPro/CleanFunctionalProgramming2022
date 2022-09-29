module HW1
import StdEnv

/* Please write your name and Neptun code here - 
	Name: MARTINS Alfredo
	Neptune-code: HEIOPO
	Date: 24.09.2022
*/

/* Student recently learned Fibonacci numbers, but he got bored of the problems.
	So, he decided to try his own new sequence:
		f(0) = a
		f(1) = b
		f(n) = f(n-1) ^ f(n-2) where ^ denotes bitxor operation (Hint: bitxor operator can be found in the documentation)
	Given a, b, and n, make a function to help him calculate the nth number in his sequence.
	For example:
	
*/

diffFibonacchi :: Int Int Int -> Int
diffFibonacchi a b 0 = a
diffFibonacchi a b 1 = b
diffFibonacchi a b n
| n < 0 = abort "n can not be negative. Please, choose other number."
= diffFibonacchi a b (n-1) bitxor diffFibonacchi a b (n-2)

//Start = diffFibonacchi 86 77 15 // = 86
//Start = diffFibonacchi 7 111 20 // = 104
//Start = diffFibonacchi 7 9 8 // = 14
//Start = diffFibonacchi 123 321 11 // = 314

/* Given three numbers, determine if any of the two numbers addition's sum of digits produces a lucky number.
A lucky number is a number whose sum of digits is divisible by 3 */

sum_digits :: Int -> Int
sum_digits x 
| x < 10 = x
= (x rem 10) + sum_digits (x/10)

isThereLucky :: Int Int Int -> Bool
isThereLucky x y z = ((sum_digits (x+y)) rem 3 == 0) || ((sum_digits (x+z)) rem 3 == 0) || ((sum_digits (y+z)) rem 3 == 0)

//Start = isThereLucky 1 3 1 // False
//Start = isThereLucky 12 21 5// True