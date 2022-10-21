module rizy_before_midterm
import StdEnv

/*1-
    Write a function that takes a list of lists
    and a function. This function returns True if
    the length of each list and its position in the list of lists
    have the same output of the function
    example: [[1,2,1], [1,4]] isEven
    returns True because length of [1,2,1] is 3 and its position
    is 1 so isEven gives False for both of them, and same for [1,4]
    Note: position starts from 1 in this task, and the "a" below means that the given list can be of any type.
*/

matchIndex :: [[a]] (Int -> b) -> Bool | == b
matchIndex list function = foldr (&&) True ([ (function (length x)) == (function i) \\ x <- list & i <- [1..(length list)] ])

//Start = matchIndex [[1,2,1], [1,4]] isEven // True
//Start = matchIndex [[1,2,1], [1,4]] (\x = x+1) // False
//Start = matchIndex [["a"], ["a", "b"]] ((+)5) //True
// Start = matchIndex [[1.0,6.0,11.0]] isOdd // True