module midterm3

import StdEnv 


// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & mid-term
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> 
//for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither made it public.
//Students’ regulation of Eotvos Lorand University 
//(ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, 
//it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be 
//dismissal of the student from the University.


/* 1. Parasitic Number

 A Parasitic number (in base 10) is a positive number which can be multiplied 
 by a certain n by moving the rightmost digit of its decimal representation 
 to the front.
 e.g. 102564 × 4 = 410256
 Given a positive integer number and n, write a function to determine whether 
 it is a Parasitic number or not.
*/


parasiticAux :: Int -> [Int]
parasiticAux x = [last num] ++ (init num)
where num = reverse(digits x)

parasitic :: Int Int -> Bool
parasitic x y = [(reverse(digits (x*y)))] == [parasiticAux x]

//Start = parasitic 102564 4 // True

//Start = parasitic 102564 4 // True
//Start = parasitic 142857 5 // True
//Start = parasitic 714285 8 // False
Start = parasitic 105263157894736842 2 // True

/* 2. Double Ones

 Given a list of integers, write a function which will keep only the numbers
 that contain at least two '1' digits. For example:
 [1,2,21,121,11,234131,111111,123,0,334] -> [121,11,234131,111111]
*/


numberOfOnes :: [Int] -> Int
numberOfOnes x = length (filter ((==) 1) x)

containsTwoOnes :: Int -> Bool
containsTwoOnes x = (numberOfOnes (digits x) >= 2)

doubleOne :: [Int] -> [Int]
doubleOne x = filter containsTwoOnes x

//Start = doubleOne [1,2,21,121,11,234131,111111,123,0,334] // [121,11,234131,111111]
// Start = doubleOne [12,1,11,33] // [11]
// Start = doubleOne [11,111,21] // [11,111]
// Start = doubleOne [] // []
// Start = doubleOne [21,3,1] // []


/* 3. Multiples

 Given an n>0 integer value, write a function that creates the double, the triple
 and so on n-th multiple of the number.
*/

multiple :: Int -> [Int]
multiple x = [x*i \\ i <- [2..x]]

//Start = multiple 5 // [10,15,20,25]
//Start = multiple 2 // [2]
//Start = multiple 1 // []


/* 4. List difference
 
 Given two lists (A and B) containing sublists of integer numbers, 
 both A and B are of the same length,
 for every sublist in A and B, return the difference of the two sublists.  

 The difference is defined as follows:  
 The List L1-L2 consists of elements that are in L1 but not in L2. 
 For example if L1=[1,2,3] and L2=[3,5], then L1-L2=[1,2].
*/

diff :: [Int] [Int] -> [Int]
diff x y = [ a \\ a <- x | not (any ((==) a) y)]

difference :: [[Int]] [[Int]] -> [[Int]] 
difference x y = [diff a b \\ a<- x & b <- y]

//Start = difference [[1..5]] [[4..7]] // [[1,2,3]]
//Start = difference [[1..10] , [10..15] , [1..4]] [[1..10] , [11..20] , [5]] // [[],[10],[1,2,3,4]]
//Start = difference [] [] // [] 

 
/* 5. Replace middle

 Given a list of lists of integers and an integer, write a function that replaces 
 the middle element with the given integer in every sublist. 
*/

replace :: [Int] Int -> [Int]
replace [] _ = []
replace x y = (take ((len/2)) x) ++ [y] ++ (drop ((len/2) + 1) x)
where len = (length x)

repMid :: [[Int]] Int -> [[Int]]
repMid x y = [(replace a y) \\ a <- x]

//Start = repMid [[1,2,3],[1..4]] 10 // [[1,10,3],[1,2,10,4]]
//Start = repMid [[1..6], [9,8..1], [(-1),(-2)..(-10)]] 5 
          // [[1,2,3,5,5,6],[9,8,7,6,5,4,3,2,1],[-1,-2,-3,-4,-5,5,-7,-8,-9,-10]]
//Start = repMid [[1,3],[]] 5 // [[1,5],[5]]


/* 6. Primes7

 Given a list of numbers, keep only the prime numbers that end with the digit 7
*/

contains7 :: Int -> Bool
contains7 x = (filter ((==) 7) (digits x)) <> []

primes7 :: [Int] -> [Int]
primes7 x = [ a \\ a <- x | (isPrime a) && (contains7 a)]

//Start = primes7 [1..10] // [7]
//Start = primes7 [1..100] // [7,17,37,47,67,97]
//Start = primes7 [1..6] // []


/* 7. Property check

 Given a list of tuples, write a function to determine
 whether all of the tuples inside of the list hold the (Even, Odd) property.
 [(2,1),(2,3),(4,1)] = True
*/

EvenOdd :: (Int, Int) -> Bool
EvenOdd (a, b) = (isEven a) && (isOdd b)

holdsTrue :: [(Int, Int)] -> Bool
holdsTrue x = all EvenOdd x

//Start = holdsTrue [(2,1),(2,3),(4,1)] // True
//Start = holdsTrue [(1,3),(2,3),(3,4)] // False
//Start = holdsTrue [] // False


/* 8. Super Digit

 We define super digit of an integer x using the following rules.
 If x has only 1 digit, then its super digit is x.
 Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.
 Here, the digit-sum of a number is defined as the sum of its digits.

 E.g  : super_digit(9875) = super_digit(9+8+7+5) 
                          = super_digit(29) 
                          = super_digit(2+9)
                          = super_digit(11)
                          = super_digit(1+1)
                          = super_digit(2)
                          = 2

 Given a list of integers, return a list containing the super digit
 of every number in the list.  
*/

digits :: Int -> [Int]
digits x
| x < 10 = [x]
= [x rem 10] ++ digits (x/10)

super_digit_aux :: Int -> Int
super_digit_aux x
| y < 10 = y
= super_digit_aux y
where y = sum (digits x)

super_digit :: [Int] -> [Int]
super_digit x = [super_digit_aux y \\ y<- x]

//Start = super_digit [148148148 , 9875 ] // [3,2]
//Start = super_digit [884555 , 456 , 2351 , 21587 , 88 ] // [8,6,2,5,7]
//Start = super_digit [] // [] 


/* 9. Powers 
 Given a list of integers and an integer, write a function which returns a list 
 which only contains the powers of the integer.
*/

isPower :: Int Int -> Bool
isPower x y = y rem x == 0

powersList :: [Int] Int -> [Int]
powersList x u = filter (isPower u) x

//Start = powersList [2,4,8,16,32,33,55] 2 // [2,4,8,16,32]
//Start = powersList [] 3 // []
//Start = powersList [1..10] 3 // [1,3,9]
//Start = powersList [-1,-2,4,8] 4 // [4]


/* 10. Twin primes
 
 Twin primes is a pair of primes, such that it contains a prime number that is either 
 2 less or 2 more than the pair prime number.
 For example, (41, 43) is a twin prime pair.
 Given a range of numbers left..right write a function that returns the count of 
 twin primes within the range.

 E.g: between 1 and 50 there are 6 pairs of twin prime numbers:
 [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43)].
*/

isDiffTwo :: (Int, Int) -> Bool
isDiffTwo (x, y) = (y-x) == 2

isPrime :: Int -> Bool
isPrime x = [ a \\ a <- [2..(x-1)] | x rem a == 0] == [] && x>1

allCom :: Int Int -> [(Int, Int)]
allCom x y = [(a, b) \\ a <- u, b <- u | a == (b-2) && (isPrime a) && (isPrime b)]
where u = [x..y]

//Start = allCom 1 50

twinPrimes :: Int Int -> Int
twinPrimes a b = length (allCom a b)

//Start = twinPrimes 1 50 // 6
//Start = twinPrimes 1 1000 // 35
//Start = twinPrimes 0 2 // 0
//Start = twinPrimes 0 -5 // 0

