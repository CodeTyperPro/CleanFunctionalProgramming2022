module ex2_todo2


import StdEnv


l1 :: [Int]
l1 = [1, 2, 3, 4, 5]
l2 :: [Bool]
l2 = [True, False, True]
l3 :: [Real->Real]
l3 = [sin, cos, sin]
l4 :: [[Int]]
l4 = [[1, 2, 3], [8, 9]]
l5 :: [a]
l5 = []
l6 :: [Int]
l6 = [1..10]
l7 :: [Int]
l7 = [1..]

//Start = l4


// Generator examples

/*Start = [1..10]     
  Start = [1,2..10]   
  Start = [1,0.. -10] 
  Start = [1.. -10]   
  Start = [1..0]      
  Start = [1..1]      
  Start = [1,3..4]  
  Start = [1..]       
  Start = [1,3..]     
  Start = [100,80..]  */

// Start = [1,2..10]

// Built-in funtions
 
// Start = hd [1, 2, 3, 4, 5]       
// Start = tl [1, 2, 3, 4, 5]      
// Start = drop 2 [1, 2, 3, 4, 5]   
// Start = take 2 [1, 2, 3, 4, 5]   
// Start = [1, 2, 3] ++ [6, 7]      
// Start = reverse [1, 2, 3]        
// Start = length [1, 2, 3, 4]      
// Start = last [1, 2, 3]           
// Start = init [1, 2, 3]           
// Start = isMember 2 [1, 2, 3]     
// Start = isMember 5 [1, 2, 3]     
// Start = flatten [[1,2], [3, 4, 5], [6, 7]]  

// Start = take 2 []                
// Start = drop 5 [1,2,3]           
// Start = take 2 [1 .. 10]         
// Start = drop ([1..5]!!2) [1..5]  

// Start = reverse [1,3..10]           
// Start = reverse [5,4 .. -5]         
// Start = isMember 0 []               
// Start = isMember -1 [1..10]         
// Start = isMember ([1..5]!!1) [1..5] 


// Exercises
// All the exercises are to be solved without map



// 1. Evaluate the following expressions:

//Start = (take 3 [1..10]) ++ (drop 3 [1..10])

//Start = length (flatten [[1,2], [3], [4, 5, 6, 7], [8, 9]])

//Start = isMember (length [1..5]) [7..10]

//Start = [1..5] ++ [0] ++ reverse [1..5]



// 2. Generate lists 

// from -10 to 10
//Start = [-10..10]


// even numbers from 1 to 100
//Start = [2,4 .. 100]


// every 10th element from 0 to 100
//Start = [0, 10 .. 100]


// Generate a list with every fifth element form 0 to 500.
//Start = [0,5 .. 500]


// Generate a list with every 500th element form -10000 to 10000.
//Start = [ -10000, -9500 .. 10000]



// 3. sumsq n returns 1*1 + 2*2 + ... + n*n - with a pattern for 0
sumsq :: Int -> Int
sumsq 0 = 0
sumsq n = n*n + sumsq (n-1)

//Start = sumsq 3 // 14


// version 2. - without pattern for 0
sums :: Int -> Int
sums n
| n == 0 = 0
= n*n + sums (n-1)

//Start = sums 3
  


// 4. Compute for a given positive n the sum of 2i*(2i+1), for i from 1 to n. E.g. for n=3 the sum is 68.
f :: Int -> Int
f 0 = 0
f n = 2*n*(2*n + 1) + f (n-1)

//Start = f 3

// f 3
// 2*3*(2*3+1) + f 2
// 2*3*(2*3+1) + 2*2*(2*2+1) + f 1
// 2*3*(2*3+1) + 2*2*(2*2+1) + 2*1*(2*1+1) + f 0
// 2*3*(2*3+1) + 2*2*(2*2+1) + 2*1*(2*1+1) + 0
// 68



// 5. Compute the sum 1+ 2*2+ 3*3*3+ 4*4*4*4+ 5*5*5*5*5+ ...+n*n*n*...*n 
// where n is a positive number.
sumpowers :: Int -> Int
sumpowers 0 = 0
sumpowers n = n^n + sumpowers (n-1)

//Start = sumpowers 3

// sumpowers 3
// 3^3 + sumpowers 2
// 3^3 + 2^2 + sumpowers 1
// 3^3 + 2^2 + 1^1 + sumpowers 0
// 3^3 + 2^2 + 1^1 + 0
// 27+4+1
// 32
//Start = sumpowers 5
//Start = sumpowers 0



// 6. write a funtion which returns true if a is divisible by b
div_by :: Int Int -> Bool
div_by a b = a rem b == 0

//Start = div_by 16 4      // True



// 7. write a funtion which returns true if a is divisible by b or vice versa
div_any :: Int Int -> Bool
div_any a b = div_by a b || div_by b a 

//a rem b == 0 || b rem a == 0

//Start = div_any 4 16     // True



// 8. Cut a list in two parts at the middle. 
// E.g. cut [1..10] -> [[1,2,3,4,5],[6,7,8,9,10]]
// and for cut [1..11] the result is [[1,2,3,4,5],[6,7,8,9,10,11]].
cut :: [Int] -> [[Int]]
cut list = [ take y list,  drop y list]
where y = length list / 2

//Start = cut [1..10]
//Start = cut [1..11]
//Start = cut []
//Start = cut [1]



// 9. Test if a list is symmetrical
sim :: [Int] -> Bool
sim list = list == reverse list

//Start = sim [1, 2, 1]
//Start = sim [1, 2, 3,4,5]



// 10. Extract the middle element of a non-empty list. E.g. for [1..5] is 3, for [1..4] is 3.
middle :: [Int] -> Int
middle x = x !! ( length x / 2)

//Start = middle [1..5] 
//Start = middle [1..4] 
//Start = middle [1]
//Start = middle []



// 11. add 3 to every element of a list
f1 :: [Int] -> [Int]
f1 [] = []
f1 [ x:xs] = [x+3 : f1 xs]

//Start = f1 [1,5,3,1,6]  // [4,8,6,4,9]  



// 12. compute the double of the positive elements of a list [1, 2, -2, 3, -4] -> [2, 4, 6]
f2 :: [Int] -> [Int]
f2 [] = []
f2 [x:xs] 
| x > 0 = [2*x : f2 xs]
= f2 xs

//Start = f2 [1, 2, -2, 3, -4] // [2, 4, 6]
// [ 1*2 : f2 [2, -2, 3, -4]]
// [ 1*2 : [2*2 : f2 [ -2, 3, -4]]]
// [ 1*2 : [2*2 : f2 [ 3, -4]]]
// [ 1*2 : [2*2 : [3*2 : f2 [ -4]]]]
// [ 1*2 : [2*2 : [3*2 : f2 []]]]
// [ 1*2 : [2*2 : [3*2 : []]]]
// [2,4,6]

// 13. write a function that keeps the integers of a list up to the first 0 encounterred 
// and then divides by 2 every element [1, 2, -2, 3, 0, -4] -> [0, 1, -1, 1]
// hints: use takeWhile then map
fa :: [Int] -> [Int]
fa [] = []
fa [x:xs] 
| x == 0 = []
= [ x : fa xs]

//Start = fa  [1, 2, -2, 3, 0, -4]
div2 :: [Int] -> [Int]
div2 [] = []
div2 [x:xs] = [ x/2 : div2 xs]

f3 :: [Int] -> [Int]
//f3 list = div2 (fa list)
f3 [] = []
f3 [ x : xs]
| x == 0 = []
= [ x/2 : f3 xs]

//Start = f3 [1, 2, -2, 3, 0, -4] // [0, 1, -1, 1]



// 14. write a function for the square of every element of a list and sublists
// [[1,2],[3,4,5,6],[7,8]]  -> [[1,4],[9,16,25,36],[49,64]]  
sq :: [Int] -> [Int]
sq [] = []
sq [ x:xs] = [x*x : sq xs]

//Start = sq [1..5]

f4 :: [[Int]] -> [[Int]]
f4 [] = []
f4 [ x : xs] = [ sq x : f4 xs]

//Start = f4 [[1,2],[3,4,5,6],[7,8]] // [[1,4],[9,16,25,36],[49,64]]



// 15. Replicate n>0 times the element of a list e.g. n=3 [3..6] ->
// [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
faux :: Int Int -> [Int]
faux 0 x = []
faux n x = [ x : faux (n-1) x]
//Start = faux 3 4

f5 :: Int [Int] -> [[Int]]
f5 n [] = []
f5 n [x:xs] = [ faux n x : f5 n xs]
//Start = f5 3 [3..6]



// 16. Compute the product of the elements of a list
product :: [Int] -> Int
product [] = 1
product [x:xs] = x * product xs

//Start = product [1,4,9] //[1..5] // 120   // prod

fact n = prod [1..n]
//Start = fact 5

// 17. delete the elements equal to 5
not_five :: [Int] -> [Int]
not_five [] = []
not_five [x:xs]
| x == 5 = not_five xs
= [ x : not_five xs]

//Start = not_five [5,4,5,4,3]  // [4,4,3]

not_five2 :: [Int] -> [Int]
not_five2 [] = []
not_five2 [5 : xs] = not_five2 xs
not_five2 [x : xs] = [x : not_five2 xs]

//Start = not_five2 [5,4,5,4,3]  // [4,4,3]

// not_five2 [5,4,5,4,3]
//            x  xs
// not_five2 [4,5,4,3]
// [4 : not_five2 [5,4,3]]
// [4 : not_five2 [4,3]]
// [4 : [ 4: not_five2 [3]]
// [4 : [ 4: [3 : not_five2 []]]
// [4 : [ 4: [3 :  []]]
// [4,4,3]



// 18. Delete an element n from a list
del :: Int [Int] -> [Int]
del n [] = []
del n [x:xs]
| x == n = del n xs
= [ x : del n xs]

//Start = del 5 [1, 5, 6, 7, 5, 8, 5] // [1, 6, 7, 8]



// 19. Keep the first 2 and the last 2 elements of a list
droptake2 :: [Int] -> [Int]
droptake2 x 
| length x < 4 = []
= take 2 x ++ drop ((length x)-2) x

//Start = droptake2 [1, 2, 3, 4, 5, 6, 7, 8, 9]
//Start = droptake2 [1, 2]



// 20. Delete the first and the last element of a list.
del_firstlast :: [Int] -> [Int]
del_firstlast x = init (tl x) 
//del_firstlast [x,y:t] = init [y:t]
//del_firstlast [x : [y:t]] = init [y:t]
//del_firstlast [x:t] = init t

//Start = del_firstlast [1..10]
//Start = del_firstlast [1]

del_firstlast2 :: [Int] -> [Int]
//del_firstlast2 x = take ((length x)-2) (drop 1 x)
del_firstlast2 x = drop 1 (take ((length x)-1) x)

//Start = del_firstlast2 [1..10]
//Start = del_firstlast2 []
//Start = del_firstlast2 [1]

delsec :: [Int] -> [Int] 
delsec [x,y:t] = [x:t]

//Start = delsec [1..10]

// 21. Rewrite flatten with ++
//Start = flatten [[1, 2, 3], [3, 4], [5, 7, 8, 9]]

lc :: [[Int]] -> [Int]
lc [] = []
lc [x:xs] = x ++ lc xs

//Start = lc [[1, 2, 3], [3, 4], [5, 7, 8, 9]]
// lc [[1, 2, 3], [3, 4], [5, 7, 8, 9]]
//      x         xs
// [1, 2, 3] ++ lc [[3, 4], [5, 7, 8, 9]]
// [1, 2, 3] ++ [3, 4] ++ lc [[5, 7, 8, 9]]
// [1, 2, 3] ++ [3, 4] ++ [5, 7, 8, 9] ++ lc [] 
// [1, 2, 3] ++ [3, 4] ++ [5, 7, 8, 9] ++ [] 
// [1, 2, 3, 3, 4, 5, 7, 8, 9]




// 22. Operations with lists: write functions for the followings
// keep the head of every sublist e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [1, 3, 5]
heads :: [[Int]] -> [Int]
heads [] = []
heads [x:xs] = [hd x : heads xs]

//Start = heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]

//heads [[1, 2, 3], [3, 4], [5, 7, 8, 9]]
//         x          xs
// [1 : heads [[3, 4], [5, 7, 8, 9]]]
// [1 : [3 : heads [[5, 7, 8, 9]]]]
// [1 : [3 : [5 : heads []]]]
// [1 : [3 : [5 : []]]]
// [1,3,5]





// 23. Keep the tails of a list in 2 versions 
// e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9]] -> [[2, 3], [4], [7, 8, 9]] 
tails :: [[Int]] -> [[Int]]
tails [] =  []
tails [ x:xs ] = [ tl x : tails xs]

//Start = tails [[1, 2, 3], [3, 4], [5, 7, 8, 9]]

tailsd :: [[Int]] -> [[Int]]
tailsd [] =  []
tailsd [ x:xs ] = [drop 1 x : tailsd xs]

//Start = tailsd [[1, 2, 3], [3, 4], [5, 7, 8, 9]]



// 24. Reverse every sublist of a list 
revsub :: [[Int]] ->  [[Int]]
revsub [] = []
revsub [x:xs] = [ reverse x : revsub xs]

//Start = revsub [[1,2,3],[5,6],[],[7,8,9,10]]



// 25. Keep the last elements of the sublists of a list in one list (the sublists are not empty).
// [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]
lasts :: [[Int]] -> [Int]
lasts [] = []
lasts [x:xs] = [ last x : lasts xs]
//lasts [x:xs] = [ hd (reverse x) : lasts xs]

//Start = lasts [[1,2,3],[5,6],[1],[7,8,9,10]]



// 26. Insert 0 in front of every sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
ins0 :: [[Int]] -> [[Int]]
ins0 [] = []
ins0 [x:xs] = [ [0] ++ x : ins0 xs]

//Start = ins0 [[1,2,3],[5,6],[],[7,8,9,10]]

ins02 :: [[Int]] -> [[Int]]
ins02 [] = []
ins02 [x:xs] = [ [0 : x] : ins02 xs]

//Start = ins02 [[1,2,3],[5,6],[],[7,8,9,10]]



// 27. Delete the last element of each sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[1,2],[5],[],[7,8,9]]
lastdel :: [[Int]] -> [[Int]]
lastdel [] = []
lastdel [x:xs] = [init x : lastdel xs]

//Start = lastdel [[1,2,3],[5,6],[],[7,8,9,10]]



// 28. write a funtion with the patterns depending on the parameter:
// if the param is [] then is equal to 20, if is a two element list starting with 4 then is 30
// if is a two element list ending with 5 then is 40, in all other cases is 50, 
// the order of the patterns is important
gp :: [Int] -> Int
gp [] = 20
gp [4,_] = 30    // gp [4,x]
gp [_, 5] = 40   // gp [x,5]
gp _ = 50        // gp x = 50

//Start = gp [4, 6] // 30
//Start = gp [4, 5] // 30
//Start = gp [1..10] //



// 29. check if a number is palindrom e.g.12321
p :: Int -> [Int]
p x = digits x []

digits :: Int [Int] -> [Int]
digits 0 x = x
digits n x = digits (n/10) [n rem 10 : x]

//Start = digits 12321 []
// digits 12321 []
// digits 1232 [1:[]]
// digits 123 [2:[1:[]]]
// digits 12 [3:[2:[1:[]]]]
// digits 1  [2:[3:[2:[1:[]]]]]
// digits 0  [1:[2:[3:[2:[1:[]]]]]] = digits 0 [1,2,3,2,1]
// [1,2,3,2,1]

pali :: Int -> Bool
pali x = y == reverse y
//where y = p x
where y = digits x []

//Start = pali 12321 // True
//Start = pali 12345



// 30. filter the elements smaller then n, e.g. n=3 [1,5,3,2,1,6,4,3,2,1] -> [1,2,1,2,1]
f7 :: Int [Int] -> [Int]
f7 n [] = []
f7 n [ x:xs]
| x < n = [x : f7 n xs]
= f7 n xs

f72 n list = filter (\ x = n > x) list
f722 n x = filter ((>) n) x    // (n > x)
f7222 n x = filter (\ x = n > x) x
cond n x = n > x
f72222 n x = filter (cond n) x

//Start = f72222 3 [1,5,3,2,1,6,4,3,2,1] 



// 31. using notempty eliminate the empty lists: 
// [[1,2,3],[],[3,4,5],[2,2],[],[],[]] -> [[1,2,3], [3,4,5], [2,2]]

notempty :: [Int] -> Bool
notempty x = not (x == [])   // x <> []    ~x  x-y
 
f8 :: [[Int]] -> [[Int]]
f8 [] = []
f8 [ x: xs]
| notempty x = [ x : f8 xs]
= f8 xs

Start = f8 [[1,2,3],[],[3,4,5],[2,2],[],[],[]]

// []   [[]] 
// [[], [], [] ]
