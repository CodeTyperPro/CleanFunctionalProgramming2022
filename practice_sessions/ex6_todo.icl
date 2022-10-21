module ex6_todo
import StdEnv

// NEPTUNE - HEIOPO - MARTINS Alfredo

// 1. Generate 100 even numbers using list comprehensions
l1 :: [Int]
l1 = take 100 ([ x \\ x <- [1..] | isEven x])

//Start = l1


// 2. Generate teh following list [4, 16, 36, 64, 100, 144, 196, 256, 324, 400]
l2 :: [Int]
l2 =  take 10 [ a*a \\ a <- [2..] | isEven a]

//Start = l2


// 3. List powers of 2 from 1 to 10.
//hint: use x^y (x at power y)
l3 :: [Int]
l3 = [ 2^i \\ i<- [1..10]] 

//Start = l3


// 4. List the divisors of 90.
l4 :: [Int]
l4 = [ i \\ i<- [1..90] | 90 rem i == 0] 

//Start = l4


// 5. List �dominoes�: [(0,0),(0,1),(1,1),(0,2),(1,2),(2,2),...,(9,9)]
// Domino (1,0) is not in the list because it is already in it as (0,1).
//l5 :: [(Int, Int)]
l5 = [ (a, b) \\  b<- [0..9], a<- [0..b]]

//Start = l5


// 6. Construct the list [(1,'a'),(2,'b'),�(�,'z')]
l6 :: [(Int, Char)]
l6 = [(a, b) \\ a<- [1..] & b<-['a'..'z']]

//Start = l6


// 7. Generate a list of length 10 whose elements are False, True, False, True, � (alternating)
l7 :: [Bool]
l7 = [ i rem 2 == 0 \\ i<- [1..10]]

//Start = l7


// 8. Is 123457 a prime number?
l8 :: Bool
l8 = [ x \\ x<- [2..123456] | 123457 rem x == 0] == []

//Start = l8


// 9. Generate the list [(0,10),(1,9),�,(10,0)].
l9 :: [(Int, Int)]
l9 = [ (a, b) \\ a<- [0..10] , b<- [10,9..0]] 

//Start = l9


//10. Generate a list that contains all (hour, minute) pairs in a day.
//l10 :: [(Int, Int)]
l10 = [ (a, b) \\ a<- [0..23] , b<- [0..59]] 

//Start = l10


// 11. (bonus point) Generate a list that contains all (month, day) pairs in a 365-day 

getDays :: Int -> Int
getDays x
| (x == 4 || x == 6 || x == 9 || x == 11) = 30
| x == 2 = 28 // Can not be 29, because in total we'll have 366 rather than 365
= 31

l11 :: [(Int, Int)]
l11 = [ (a, b) \\ a<- [1..12] , b<- [1..(getDays a)]]

//Start = l11