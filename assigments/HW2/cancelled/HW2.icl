module HW2
import StdEnv


/* Please write your name and Neptun code here -              */

/* 1. Given a list of numbers, multiply every even number of the list by 4,
and every odd number of the list by 5  */

my_multiplication :: [Int] -> [Int]
my_multiplication [] = []
my_multiplication [x:xs] 
| x rem 2 == 0 = [x*4 : my_multiplication xs]
= [x*5 : my_multiplication xs]

//Start = my_multiplication [14, 22, 45, 56] // [56,88,225,224]
//Start = my_multiplication [13, 27, 44] // [65,135,176]
//Start = my_multiplication [] // []

/* 2. Define a function join that takes a list, and pairs the first with the last,
the second with the second last and so on in a sublist, resulting in lists of list.
If the list has odd length, the middle element is paired with copy of itself.

Example: [1,2,3,4,5,6] = [[1,6], [2,5], [3,4]] */

join :: [Int] -> [[Int]]
join [] = []
join xs = [[hd xs, last xs]] ++ join (init (tl xs))

//Start = join [1,2,3,4,5] // [[1,5], [2,4], [3,3]
//Start = join [] // []
//Start = join [1,4,8,16,22,27] // [[1,27], [4,22], [8,16]]



