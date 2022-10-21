module HW4
import StdEnv

/*
write a function that takes a matrix as list of lists and returns the transpose of the matrix
eg :
Input : [[1,2,3],
         [4,5,6],
         [7,8,9]]
Output : [[1,4,7],
          [2,5,8],
          [3,6,9]]
If there is an empty list or a list of different length, return an empty list

***Needs to use atleast one higher order function***
*/

funTran :: [[a]] -> [[a]]
funTran [] = []
funTran [x:xs]
| length x > 0 = [map hd [x:xs]] ++ (funTran (map tl [x:xs]))
= []

//Start = funTran [[1,2,3],[4,5,6],[7,8,9]] // [[1,4,7],[2,5,8],[3,6,9]]
//Start = funTran [[1,2],[3,4],[5,6]] // [[1,3,5],[2,4,6]]
//Start = funTran [['a','b','c'],['d','e','f'],['g','h','i']] // [['a','d','g'],['b','e','h'],['c','f','i']]


/*
Write a function that takes two matrices and returns the sum of the two matrices
If the matrices are not of the same order, return an error message
eg :
Input : [[1,2,3],     [[1,2,3],
         [4,5,6],      [4,5,6],
         [7,8,9]]      [7,8,9]]
Output : [[2,4,6],
          [8,10,12],
          [14,16,18]]
Needs to use atleast one higher order function
*/

containEmpty :: [[Int]] -> Bool
containEmpty list = (filter ((==) []) list) <> []

sum_lists :: [Int] [Int] -> [Int]
sum_lists x y = [ (a+b) \\ a <- x & b <- y]

funSum :: [[Int]] [[Int]] -> [[Int]]
funSum x y
| len_x <> len_y || containEmpty x || containEmpty y = abort "Error"
= [sum_lists a b \\ a<- x & b <- y]
where
	len_x = length x
	len_y = length y


//Start = funSum [[1,2,3],[4,5,6],[7,8,9]] [[1,2,3],[4,5,6],[7,8,9]] // [[2,4,6],[8,10,12],[14,16,18]]
//Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] // [[2,4],[6,8],[10,12]]
//Start = funSum [[1,2],[3,4],[5,6]] [[1,2],[3,4]] // Error
Start = funSum [[],[1,3]] [[1,2],[4,5]] // Error
