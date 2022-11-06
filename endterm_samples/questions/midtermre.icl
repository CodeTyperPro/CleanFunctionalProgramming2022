module midtermre
import StdEnv

// For mark 2: 4 exercises, mark 3: 6 exercises,
// mark 4: 8 exercises, mark 5: 10 (all) exercises must be solved

// 1. Insert x as first element in every sublist of a list.
// if the sublist was empty then x will be the only element in the new sublist.
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100 -> [[100,1,2], [100,3,4,5], [100,6,5,9,7], [100], [100,8]]

f1 :: [[Int]] Int -> [[Int]]
f1 l x = map (\a=[x]++a) l
//Start = f1 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100

// 2. Increase by 10 all the elements of the sublists of a list (you must use map)
// [[1, 2, 3], [3, 4], [5, 7, 8, 9], [], [1..5]] -> [[11, 12, 13], [13, 14], [15, 17, 18, 19], [], [11,12,13,14,15]]

f2 :: [[Int]] -> [[Int]]
f2 [] = []
f2 [h:t] = [map (\a = a+10) h: f2 t]

//Start = f2 [[1, 2, 3], [3, 4], [5, 7, 8, 9], [], [1..5]]

// 3. Check if a list contains 2 equal elements one after the other
// (they can be anywhere in the list) and count such equalitites
// for [1,2,2,3,4,3,3,2,4,5,5,5] is 4 for [1 .. 5] is 0.
f3 :: [Int] -> Int
f3 [] = 0
f3 [h] = 0
f3 [h,x:t]
| h == x = 1 + f3 [x:t]
= f3 [x:t]
//Start = f3 [1,2,2,2,3,4,3,3,2,4,5,5,5,5,5,5,5,5,5]

// 4. Extract the second element of the sublists (if there is no such element, ignore that sublist)
// [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]] -> [2,4,7,9]
f4 :: [[Int]] -> [Int]
f4 x = map (\a = a!!1)(filter (\a = length a >= 2) x)
//Start = f4 [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]]

// 5. Insert an x as second element into every sublist of a list.
// (if the sublist is shorter then just add x to it)
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10 -> [[1,10,2], [3,10,4,5], [6,10,5,9,7], [10], [8,10]]
insert_2 :: [Int] Int -> [Int]
insert_2 [] n = [n]
insert_2 x n = insertAt 1 n x
//Start = insert_2 [3,4,5] 2

f5 :: [[Int]] Int -> [[Int]]
f5 x n = map (\k = insert_2 k n) x
//Start = f5 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10

// 6. Using foldr compute the sum of the elements of the sublists and add them as last elements.
// [[1..5],[1..3],[5,1,2],[],[3]] -> [[1,2,3,4,5,15],[1,2,3,6],[5,1,2,8],[0],[3,3]]

f6 :: [[Int]] -> [[Int]]
f6 [] = []
f6 x = map (\k=k++(map (foldr (+) 0) [k])) x
//Start = f6 [[1..5],[1..3],[5,1,2],[],[3]]

// 7. Eliminate the elements up to the first 0, and compute the product of the positive elements of the rest.
f7 :: [Int] -> Int
f7 x =  foldr (*) 1 (filter ((<)0) (dropWhile ((<>)0) x))

//Start = f7 [1,-2,-4,5,0,1,-6,1,-1,-2,5,0,2] //10

// 8. Ignore the first x elements of the sublists and then sum up them.

f8 :: Int [[Int]] -> [Int]
f8 x ls = map (foldr (+)0) (map (drop x) ls)
//Start = f8 3 [[1..5], [], [1..4], [1,5,1], [], [1,2,4,50,100], [1..10]] // [9,0,4,0,0,150,49]

// 9. Delete every third element of the sublists of a list.

delete_3 :: [Int] -> [Int]
delete_3 [] = [] 
delete_3 x = removeAt 2 x

f9 :: [[Int]] -> [[Int]]
f9 x = map delete_3 x

//Start = f9 [[1..5],[],[1..4],[1,5],[1],[1..3],[1..10]]

// [[1,2,4,5],[],[1,2,4],[1,5],[1],[1,2],[1,2,4,5,7,8,10]]

//10. For sublists of even lenght of a list, cut the sublists at midle and keep only the first half
//and then invert them.
f10 :: [[Int]] -> [[Int]]
f10 x = map (\a = reverse (take ((length a)/2) a)) (filter (\a = (length a) rem 2 == 0) x)
//
Start = f10 [[1..5],[1,2,2,1],[1,1,2,2,1,1],[11..16],[],[1,2,3,3,2,1]]

// [[2,1],[2,1,1],[13,12,11],[],[3,2,1]]