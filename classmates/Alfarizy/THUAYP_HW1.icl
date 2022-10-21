module THUAYP_HW1
import StdEnv 


/*

Name: Alfarizy Alfarizy
Neptun code: THUAYP

Your submission should not have any errors when running the code.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.


Make sure to rename the file by your neptun_code 
*/

/// Tasks

/*-1 
    * Roman numerals are represented by seven different symbols: I, V, X, L, C, D and M.
     Symbol       Value
        I             1
        V             5
        X             10
        L             50
        C             100
        D             500
        M             1000
    
    * Write the function "getValue" which gets a symbol and returns the represented value of that symbol according to the table above.
    * If the given symbol is not in the table above, abort with "Not valid input" 
    * hint:  you can use the abort funciton

    Example for the abort function:
    task: calculate the factorial of a positive number
          if the number is negative, abort with "invalid argument"

          solution:
          fact :: Int -> Int
          fact x
          | x <  0 = abort "invalid argument" // if the value is less than 0 -> abort the program with this message "invalid argument" 
          | x == 1 = 1 
          = x * fact (x - 1)   

          Start = fact 3 // 6 
          Start = fact 4 // 24 
          Start = fact -1 //  invalid argument" 

*/

getValue :: Char -> Int
getValue x 
| x == 'I' = 1
| x == 'V' = 5
| x == 'X' = 10
| x == 'L' = 50
| x == 'C' = 100
| x == 'D' = 500
| x == 'M' = 1000
= abort "invalid input"

// Write your code here ...


/// Used to test your funciton if it produces the correct answer or not !

//Start = getValue 'I' // 1
//Start = getValue 'V' // 5
//Start = getValue 'X' // 10 
//Start = getValue 'L' // 50
//Start = getValue 'C' // 100
//Start = getValue 'D' // 500 
//Start = getValue 'M' // 1000
//Start = getValue '0' //  "invalid input"


/*2-  
    Create a function which transforms the number of days to years, weeks and days. 
    For example: 375 days = 1 year 1 week 3 days.
    - 1 year = 365 days (Ignoring leap year)

    Hints:
         * you can use the toString function: the funciton  transforms the given input to String (e.g toString 5  ->  "5")
         * don't forget the paranthesis!
*/



transform :: Int -> String 
transform days = toString(days/365) +++ " year " +++ toString((days rem 365)/7) +++ " week " +++ toString((days rem 365) rem 7) +++ " days "

// Write your code here ...


//Start = transform 375 // "1 year 1 week 3 days"
//Start = transform 365 // "1 year 0 week 0 days"
//Start = transform 1050 // "2 year 45 week 5 days"
//Start = transform 2500 // "6 year 44 week 2 days"

/*-3
    * The first element of the Collatz series is an arbitrary positive integer. Following elements could be defined recursively:
    * the next element is 3 times the previous element increased by one, if the previous element was odd, otherwise the next
    * element is the half of the previous element.
    * 
    * As an example, starting from 3 as an input, the elements of the Collatz series are
    * 3, 10, 5, 16, 8, 4, 2, 1
    * 
    * According to the Collatz conjecture, this series **always** reaches 1, which is the last element in the previous
    * example, making the length of the Collatz series 8.

    * The function gets the first element of the series as input and should return the length of the Collatz series
*/

len :: Int -> Int
len x
| x <= 0 = abort "Invalid input"
| x == 1 = 1
| x > 1 && isEven x = 1 + len (x/2)
| x > 1 && isOdd x = 1 + len ((3*x)+1)

// Write your code here

//Start = len 3 // 8 
//Start = len 5 // 6   
//Start = len 8 // 4   
//Start = len -8 // "Invalid input"    



