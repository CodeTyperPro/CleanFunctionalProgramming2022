module Alia_1
import StdEnv

/*
  Given a matrix (2-dimensional integer list), the size of matrix and a boolean function. 
  You have to check N shape of the given matrix meets the condition.
  
  It is guaranteed that the length of row and column of the given matrix are the same.
    
    * x x *
    * * x *
    * x * *
    * x x *
  
  All the * elements meet the given boolean function.
  If it is 1x1 or 2x2 matrix, then return true if it meets the boolean function.

*/

Check :: (Int -> Bool) [[Int]] Int Int -> Bool
Check function list 0 x = function ((list!!0)!!0)
Check function list index x = function ((list!!index!!0)) && function ((list!!index!!x)) && (function ((list!!index!!index))) && (Check function list (index-1) x)

NShapeCheck :: [[Int]] Int (Int -> Bool) -> Bool
NShapeCheck list x function = Check function list (x-1) (x-1)

//Start = NShapeCheck [[2,3,4,14],[6,-12,3,0],[6,3,18,2],[10,-34,67,8]] 4 isEven // True
//Start = NShapeCheck [[2,4],[4,7]] 2 isEven // False
//Start = NShapeCheck [[3]] 1 isOdd // True
//Start = NShapeCheck [[0,3,4,0],[0,0,3,2],[0,3,0,0],[0,-34,67,0]] 4 isZero // False
//Start = NShapeCheck [] 0 isZero // True