module PT5
import StdEnv

/*
	Given a 2d list of integer and in each sublist, make a tuple of pair the even and odd number 
	[transform the sublist of integer into sublist of tuple (Int, Int)]
	It is guaranteed that every sublist has the equal number of even and odd.
	
	Pairing order can be varied, but every number should be paired only once.
*/

GetEven :: [Int] -> [Int]
GetEven x = filter isEven x

GetOdd :: [Int] -> [Int]
GetOdd x = filter isOdd x

JoinEvenOdd :: [Int] [Int] -> [(Int, Int)]
JoinEvenOdd x y = [(a, b) \\ a<- x & b<- y]

PT5:: [[Int]] -> [[(Int,Int)]]
PT5 list = [JoinEvenOdd (GetOdd x) (GetEven x) \\ x <- list]

//Start = PT5 [[34,5,92,68,23,15],[1..10]] // [[(34,5),(92,23),(68,15)],[(2,1),(4,3),(6,5),(8,7),(10,9)]]
//Start = PT5 [[5,-3,2,-8],[],[4,3]]			// [[(2,5),(-8,-3)],[],[(4,3)]]
//Start = PT5 [] // []