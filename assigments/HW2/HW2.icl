module HW2
import StdEnv


/* Please write your name and Neptun code here - MARTINS Alfredo - HEIOPO             */

/* 1.  keep the middle element of every sublist, if the length of the sublist is odd.
If it is even ignore that sublist 
 e.g. [[1, 2, 3], [3, 4], [5, 7, 8, 9], [11,12,13] -> [2, 12] */

removeMiddle :: [[Int]] -> [Int]
removeMiddle [] = []
removeMiddle [x: xs]
| (s rem 2 == 0) = removeMiddle (xs)
= [(x!!(s/2)) : removeMiddle (xs)]
where s = length x

//Start = removeMiddle [[1,27, 56, 44], [4,22, 29], [8,16, 29]]// [22, 16]
//Start = removeMiddle [[1,5], [2,4], [3,3]] // []
//Start = removeMiddle [] // []
//Start = removeMiddle [[],[],[1, 2],[]] // []
//Start = removeMiddle [[1, 6, 4, 8],[2, 11, 43],[3, 54, 434, 43]] // [11]

/* 2. Write a function gaps that gives all the possibilities to take out one element from a list. 
For example:
gaps [1,2,3,4,5] = [[2,3,4,5], [1,3,4,5], [1,2,4,5], [1,2,3,5], [1,2,3,4]] */

gapsAux :: [Int] Int -> [[Int]]
gapsAux list x
| x<1 || x > length list = []
= [(take (x-1) list) ++ (drop x list) : gapsAux list (x+1)]

gaps :: [Int] -> [[Int]]
gaps [] = []
gaps list = gapsAux list 1

//Start = gaps [1,2,3,4,5] // [[2,3,4,5], [1,3,4,5], [1,2,4,5], [1,2,3,5], [1,2,3,4]] 
//Start = gaps [] // []
//Start = gaps [1, 2] // [[2],[1]]
//Start = gaps [1, 2, 3] // [[2,3],[1,3],[1,2]]
Start = gaps [9, 5, 4, 2, 8] // [[5,4,2,8],[9,4,2,8],[9,5,2,8],[9,5,4,8],[9,5,4,2]]

