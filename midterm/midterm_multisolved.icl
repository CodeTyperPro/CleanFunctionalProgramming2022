module midterm_multisolved
import StdEnv 


/* Please fill in the data required below:
  <Name>
  <Neptun_code>
  Functional Programming: midterm
  2022.October.21
  I declare that this solution is my own work.
  I have not copied or used third party solutions.
  I have not passed my solution to my classmates, 
  neither made it public.
*/


///////////////////////////////// TASK 1 /////////////////////////////////  
/*1- Unique digits - 10 points
    Given an integer n, return the count of all unique digits of n.
    Input: n = 1232
    Output: 2 (only 1 and 3 are unique - appeared only once in n).
    Input: n = 1111
    Output: 0 (There is no unique digit in n.)
*/

toDigit :: Int -> [Int]
toDigit x 
| x < 10 = [x]
= toDigit (x/10) ++ [x rem 10]

isUnique :: Int [Int]  -> Bool 
isUnique x ls =  length (filter ((==) x) ls) == 1 

count_unique_digits :: Int -> Int 
count_unique_digits x = length (filter ((==) True ) (map (\d = isUnique d y) y))
where y = toDigit x

/////

occur :: Int [Int] Int -> Int
occur num [] i = i
occur num [x:xs] i
| num == x = occur num xs (i + 1)
= occur num xs i

count_unique_digits2 :: Int -> Int
count_unique_digits2 x = length [num \\ num<-(toDigit x) | occur num (toDigit x) 0 == 1 ]

Start = count_unique_digits 1234 // 4
// Start = count_unique_digits 12325332 // 2
// Start = count_unique_digits 111111 // 0
// Start = count_unique_digits 1 // 1


///////////////////////////////// TASK 2 /////////////////////////////////
/*2- Count good lists - 10 points
    Given a list of lists of integer numbers, count the good sublists in 
    the given list. A list is considered to be good if the numbers at 
    even positions are even and the numbers at odd positions are prime.    
    Input:  [[2,2,4,5], [2,3,3,5]]
    Output: 1 (Only the [2,2,4,5] sublist is good as the numbers at 
    0th, 2nd (even) positions are even and the numbers at 1st, 3rd (odd) 
    positions are prime.
*/

isGood :: [Int] -> Bool
isGood [] = True 
isGood [x] = isEven x
isGood [x,y:xs] = isEven x && isEmpty[s \\ s <- [2..(y-1)] | y rem s == 0 ] &&  isGood xs 

count_good_lists :: [[Int]] -> Int
count_good_lists ls =   foldr (\d y | d = y + 1  = y ) 0  (map (\subls = isGood subls) ls) 

/////

isPrime :: Int -> Bool
isPrime n= length [i\\i<-[1..n]|(n rem i)==0]==2

isGood2 :: [Int]->Bool
isGood2 xs = xs==[xs!!i\\i<-[0..(length xs)-1]|( (isEven i)&&(isEven (xs!!i)) || ((isOdd i)&&(isPrime (xs!!i))) ) ]

count_good_lists2 :: [[Int]] -> Int
count_good_lists2 xs = length [x\\x<-xs|isGood2 x]

// Start = count_good_lists [[2,2,4,5],[2,3,3,5]] // 1
// Start = count_good_lists [[2,23,22],[2,29,22,5],[1,2,3]] // 2
// Start = count_good_lists [[2,2,4,5],[2,2,6,7,8,11,12,17],[12,23,4]] // 3
// Start = count_good_lists [] // 0


///////////////////////////////// TASK 3 /////////////////////////////////  
/*3- Increase by position - 10 points
    Given a list of real numbers, add the position of every number to the number.    
    Input:  [1.0,2.1,3.5,2.0]
    Output: [1.0,3.1,5.5,5.0] (the position of 1.0 is 0 -> 1.0 + 0 = 1.0  
                               the position of 2.1 is 1 -> 2.1 + 1 = 3.1 
                               the position of 3.5 is 2 -> 3.5 + 2 = 5.15
                               the position of 2.0 is 3 -> 2.0 + 3 = 5.0)
*/

increaseByPosition :: [Real] -> [Real]
increaseByPosition x = [el + toReal i \\ el <- x & i <- [0..]] 

/////

increaseByPosition2 :: [Real] -> [Real]
increaseByPosition2 list = [el+y \\ el <- list & y <- [0.0..]]

// Start = increaseByPos [1.0,2.1,3.5,2.0] // [1,3.1,5.5,5]
// Start = increaseByPos [55.12,22.45,2.10,15.1,20.20] // [55.12,23.45,4.1,18.1,24.2]
// Start = increaseByPos [] // []


///////////////////////////////// TASK 4 /////////////////////////////////  
/*4- Reverse integers - 10 points
    Given a list of integer numbers, reverse every number in the list.
    Reversing a number means to write its digits in the reversed order. 
    Input:  [1,234,5677,43,0]
    Output: [1,432,7765,34,0] Reverse of 1 is 1    
       Reverse of 234: the digits of 234 in reversed order are 4,3 and 2, 
       and by combining these digits we get the number 432
    Note: reverse of e.g. 230 is 32 NOT 032  
*/

reverse_num :: Int -> Int 
reverse_num x = toInt ( foldr (+++) "" (map (\x= toString x) (reverse (toDigit x))) )

rev_nums :: [Int] -> [Int]
rev_nums [] = [] 
rev_nums [x:xs] = [(reverse_num x)] ++ rev_nums xs  

/////

flat :: [Int] -> Int
flat [] = 0
flat list = hd list * (10^(length (list) - 1)) + flat (tl list)

rev_nums2 :: [Int] -> [Int]
rev_nums2 list = map (\ x = flat (reverse (toDigit x))) list

// Start = rev_nums [1,234,5677,43,0] // [1,432,7765,34,0]
// Start = rev_nums [1..5] // [1,2,3,4,5]
// Start = rev_nums [222..240] 
// [222,322,422,522,622,722,822,922,32,132,232,332,432,532,632,732,832,932,42]
// Start = rev_nums [] // []


///////////////////////////////// TASK 5 ///////////////////////////////// 
/*5- Passed students - 10 points
    Given a list of tuples and an integer number (let's call it x), where 
    the first element of the tuple represents a student's name and 
    the second element of the tuple represents the points of the student 
    that he/she got in a particular subject (its type is a list of real numbers).
    Return those students whose points have the following property:
    if the sum of the INTEGER parts of the points is greater than or equal 
    to the given number x.
    Input: [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320 
    Output: ["Abdullah"] ( the sum of the integer parts of [55.55,66.55,77.75,65.07,65.57] 
                           = 55 + 66 + 77 + 65 + 65 = 328 >= 320 (the given x)
                           - the sum of the integer parts of [27.55,20.55,10.75,30.07,20.57]
                           = 27 + 20 + 10 + 30 + 20 = 107 < 320 (the given x) )
*/

passedStudents :: [(String,[Real])] Int -> [String] 
passedStudents ls x = [z  \\ z <- [name \\ (name,points) <- ls | (foldr (\v s| toReal (toInt v) < v = toInt v + s = ((toInt v) - 1) + s) 0 points ) >= x]]

/////

tointAux:: Real->Int
tointAux n
|toReal(toInt n) > n = (toInt n) - 1
= toInt n

passedStudents2 :: [(String,[Real])] Int -> [String]
passedStudents2 ls n = [s \\ (s,x)<-ls | sum (map (tointAux) x) >= n]

// Start = passedStudents [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320 // ["Abdullah"]
// Start = passedStudents [("Sara" , [5.55,44.55,55.75,30.07,90.57]),("Rayan",[56.55,66.55,7.75,77.07,77.57]),("Ali",[1.55,6.55,66.75,6.07,7.57]),("Maria",[54.55,60.55,66.75,20.07,74.57])] 200 
// ["Sara","Rayan","Maria"]
// Start = passedStudents [] 100 // []


///////////////////////////////// TASK 6 /////////////////////////////////  
/*6- Eliminate - 10 points
    Given a list of numbers eliminate the first number of 
    every two numbers in the list, until only one number is left.  
    Input: a = [1, 2, 3, 4, 5, 6, 7, 8, 9]
           a = [2, 4, 6, 8]
           a = [4, 8]
           a = [8] 
*/

aux :: [Int] Int -> [Int]
aux [] _ = []
aux list i
|isEven i = aux (tl list) (i+1)
= [hd list] ++ aux (tl list) (i+1)

eliminate :: [Int] -> [Int]
eliminate a
| length a == 1 = a
= eliminate (aux a 0)

/////

eliminate2 :: [Int] -> [Int]
eliminate2 [] = []
eliminate2 [x] = [x]
eliminate2 ls = eliminate2 [x \\ x<- ls & i <-[1..] | i rem 2 == 0] 

//Start = eliminate [1..9] // [8]
//Start = eliminate [1,2,3,4] // [4]
//Start = eliminate [0] // [0]
//Start = eliminate [] // []


///////////////////////////////// TASK 7 /////////////////////////////////  
/*7- Delete third - 10 points
    Delete every third element from a list.
*/

del3 :: [Int] -> [Int]
del3 [] = []
del3 [x] = [x]
del3 [x,y] = [x,y]
del3 [x,y,z: t] = [x,y : del3 t]

/////

del32 :: [Int] -> [Int]
del32 ls = [x \\ x <-ls & i <-[1..] | i rem 3 <> 0]

//Start = del3 [1..7]  // [1,2,4,5,7]
//Start = del3 [1..20] // [1,2,4,5,7,8,10,11,13,14,16,17,19,20]
//Start = del3 [1..5]  // [1,2,4,5]
//Start = del3 []      // []


///////////////////////////////// TASK 8 ///////////////////////////////// 
/*8- Fibonacci lists - 10 points
    Write a function that takes a list of integers and for every integer 
    returns a list of Fibonacci sequence less than or equal to the integer.
    A Fibonacci sequence is a sequence of numbers where each number is 
    the sum of the previous two numbers: 0, 1, 1, 2, 3, 5 ..... and so on
    Input: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    Output: [[0],[0,1],[0,1,1],[0,1,1,2],[0,1,1,2,3],[0,1,1,2,3,5],[0,1,1,2,3,5]
            ,[0,1,1,2,3,5,8],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8]]
*/

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

genFib :: Int Int -> [Int]
genFib x n
| n == 0 = [0]
| x >= n = []
| fib x > n = []
= [fib x] ++ genFib (x+1) n

FibList :: [Int] -> [[Int]]
FibList x = map (\ y = genFib 0 y) x

//Start = FibList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  
// [[0],[0,1],[0,1,1],[0,1,1,2],[0,1,1,2,3],[0,1,1,2,3,5],
// [0,1,1,2,3,5],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8]]
//Start = FibList [0,45,88,87,21] 
// [[0],[0,1,1,2,3,5,8,13,21,34],[0,1,1,2,3,5,8,13,21,34,55],
// [0,1,1,2,3,5,8,13,21,34,55],[0,1,1,2,3,5,8,13,21]]
//Start = FibList [] // []


///////////////////////////////// TASK 9 /////////////////////////////////  
/*9- Carry-less addition - 10 points
    Given 2 lists of integers of same length, perform carry-less addition 
    between 2 integers at the same position and return the list of integers.
    Carry-less addition: 9+7 results 6, not 16.
                        35+48 results 73, not 83                        
    Here, for the sake of simplicity, there will be only 1-digit integer.
*/

carrylessDigitAddition :: [Int] [Int] -> [Int]
carrylessDigitAddition list1 list2 = [(x+y) rem 10 \\ x <- list1 & y <- list2]

//Start = carrylessDigitAddition [9,6,3,4,3,4,5] [4,8,9,0,9,6,5] // [3,4,2,4,2,0,0]
//Start = carrylessDigitAddition [7,6,5,9,8,7,6] [9,8,7,6,3,2,9] // [6,4,2,5,1,9,5]
//Start = carrylessDigitAddition [7,8,3,8] [6,2,7,8]             // [3,0,0,6]
//Start = carrylessDigitAddition [2,3,4,3,2] [9,8,-7,6,5]        // [1,1,-3,9,7]


///////////////////////////////// TASK 10 ///////////////////////////////// 
/*10- Zip with LCM - 10 points
    Write a function that takes two lists of integers and returns a 
    list of tuples where the first element of the tuple is an element of 
    the first list and the second element of the tuple is an element 
    of the second list at the same position and the third element is the 
    LCM of the first two elements (LCM - least common multiple of two numbers).
    LCM(2,3) = 6 LCM(3,4) = 12 LCM(12,15) = 60
    If the lists are of different lengths, the function should return a list 
    of tuples of the same length as the shorter list.
*/

lcm :: Int Int -> Int
lcm x y = (x * y) / gcd x y

ZipWithLCM :: [Int] [Int] -> [(Int, Int, Int)]
ZipWithLCM [] _ = []
ZipWithLCM _ [] = []
ZipWithLCM [x:xs] [y:ys] = [(x,y,lcm x y)] ++ ZipWithLCM xs ys

/////

ZipWithLCM2 :: [Int] [Int] -> [(Int, Int, Int)]
ZipWithLCM2 x y = [(u, v, (u*v)/ gcd u v) \\ u <- x & v <- y]

//Start = ZipWithLCM [12,14,22,57,66] [13,15,17,19,21] 
// [(12,13,156),(14,15,210),(22,17,374),(57,19,19),(66,21,462)]
//Start = ZipWithLCM [78,43,12,33,65] [32,77,21,11,9,43] 
// [(78,32,1248),(43,77,3311),(12,21,84),(33,11,33),(65,9,585)]
//Start = ZipWithLCM [] [32,77,21,11,9,43] // []
//Start = ZipWithLCM [78,43,12,33,65] [] // []
//Start = ZipWithLCM [] [] // []


///////////////////////////////// TASK 11 ///////////////////////////////// 
/*11- Split number - 10 points
    Write a function that takes a number, splits in the middle and interchanges the 
    two halves. If digits' number is odd, the second half contains the middle digit.
    1234 -> 12 | 34 -> 34 | 12 -> 3412
    12345 -> 12 | 345 -> 345 | 12 -> 34512
*/

NumtoList :: Int -> [Int]
NumtoList 0 = []
NumtoList n = NumtoList (n / 10) ++ [n rem 10]

ListtoNum :: [Int] -> Int
ListtoNum [] = 0
ListtoNum [x:xs] = x * 10 ^ (length xs) + ListtoNum xs

FunNum :: Int -> Int
FunNum n = ListtoNum ((drop ((length x)/2) x) ++ (take ((length x)/2) x))
where x = NumtoList n

/////

toInteger :: [Int] -> Int
toInteger x = toInt (foldl (+++) "" (map toString x)) 

FunNum2 :: Int -> Int
FunNum2 n = toInteger ((drop ((length p)/2) p) ++ (take ((length p)/2) p))
where p = toDigit n

// Start = FunNum 0 // 0
// Start = FunNum 1234 //3412
// Start = FunNum 12345 //34512
// Start = FunNum 123456 //456123


///////////////////////////////// TASK 12 /////////////////////////////////  
/*12- Fold if true - 10 points
    Write function foldiftrue which reduces only those elements of a list which 
    satisfy a given predicate. There are 4 reduce options which are given in String:
    "max" return max number, "min" return min number, "*" return product, "+" return sum. 
*/

foldiftrue :: (Int -> Bool) String [Int] -> Int
foldiftrue pred func list
| func == "+" = sum (filter pred list)
| func == "*" = prod (filter pred list)
| func == "max" = maxList (filter pred list)
| func == "min" = minList (filter pred list)
= abort "not good reduce"

/////

foldiftrue2 :: (Int -> Bool) String [Int] -> Int
foldiftrue2 cond function list
| function == "max" = last (sort (filter cond list))
| function == "min" = hd (sort (filter cond list))
| function == "+" = foldr (+) 0 (filter cond list)
| function == "*" = foldr (*) 1 (filter cond list)
= abort "not good reduce"

// Start = foldiftrue ((>)5) "max" [6,1,2,3] // 3
// Start = foldiftrue ((>)5) "min" [6,1,2,3] // 1
// Start = foldiftrue (isEven) "+" [6,1,2,3, 233, 287] // 8
// Start = foldiftrue (isEven) "*" [6,1,2,3, 233, 287] // 12


///////////////////////////////// TASK 13 ///////////////////////////////// 
/*13- Salary calculation - 30 points this task has 3 parts, each of 10 points 
    You are given list of tuples with employees' name, age and salaries, do some analysis. 
    Find about all the given queries using functions.
    1. What is the average salary of the employees?
    2. If the employer is to deduct 15% of the salaries of employees younger than 35 years old,
       how much money would he save?
    3. Give only the list of names where the employee is older than 35 but earns more than 300.
    [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), 
     ("Marie", 55, 573), ("Lucy", 37, 400)]
    1. 424.71...
    2. 87.6
    3. ["Bob", "Anna", "Marie", "Lucy"]
*/

averageSalary :: [(String, Int, Int)] -> Real
averageSalary x = toReal (sum (map (\t = thd3 t) x)) / toReal (length x)

//Start = averageSalary [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]

savedMoney :: [(String, Int, Int)] -> Real
savedMoney x = sum (map (\y = toReal (thd3 y) * 0.15) (filter (\t = snd3 t < 35) x))

//Start = savedMoney [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]

namesOlder35 :: [(String, Int, Int)] -> [String]
namesOlder35 x = map (\y = fst3 y) (filter (\t = snd3 t > 35 && thd3 t > 300) x)

//Start = namesOlder35 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]

/////

getSalaries :: [(String, Int, Int)] -> [Real]
getSalaries list = map toReal (map thd3 list)

averageSalary2 :: [(String, Int, Int)] -> Real
averageSalary2 list = avg (getSalaries list)
//Start = averageSalary2 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)] // 424.714285714286

isOlderThan35 :: (String, Int, Int) -> Bool
isOlderThan35 t = snd3 t > 35

savedMoney2 :: [(String, Int, Int)] -> Real
savedMoney2 list = total - total*0.85
where total = foldr (+) 0.0 (map toReal (map thd3 [x \\ x <- list | not (isOlderThan35 x)]))
//Start = savedMoney2 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]//87.6

earnsMoreThan300 :: (String, Int, Int) -> Bool
earnsMoreThan300 t = thd3 t > 300

namesOlder352 :: [(String, Int, Int)] -> [String]
namesOlder352 list = map fst3 [x \\ x <- list | earnsMoreThan300 x && isOlderThan35 x]
//Start = namesOlder352 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]//["Bob", "Anna", "Marie", "Lucy"]

//////////////////////////////////////////////////////////////////