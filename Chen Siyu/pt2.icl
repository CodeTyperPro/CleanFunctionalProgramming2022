module pt2
import StdEnv

/* Enter your name and Neptun code here            */

/* Given list of numbers, keep only those numbers that are inside the interval between the
first and the last numbers of the list, and then return their squares. */

helper :: Int Int [Int] -> [Int]
helper a b [] = []
helper a b [x:xs]
| x>=a && x<=b = [x*x : helper a b xs]
= helper a b xs

f :: [Int] -> [Int]
f [] = []
f [x: xs]
| x>(last xs) = abort "Last can not be higher than the first"
= helper x (last xs) [x: xs]

//Start = f [1..5] // [1, 4, 9, 16, 25]
//Start = f [1, -1, 2, 3, 4, 900, -1] // abort as the last number should always be higher than the first number
//Start = f [-10, 4, 7, -900, -3, 0] // [100, 9, 0]