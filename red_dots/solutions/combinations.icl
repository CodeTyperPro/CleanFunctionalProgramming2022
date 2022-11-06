module combinations
import StdEnv

// Generate the combinations of K distinct objects chosen from the N elements of a list 
// In how many ways can a committee of 3 be chosen from a group of 12 people? We all(?) know that there are C(12,3) = 220 possibilities  
// But we want to really generate all the possibilities in a list. 

combinations :: Int [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n [x:xs] = (map (\y = [x] ++ y) (combinations (n-1) xs)) ++ (combinations n xs)

// Start = combinations 0 [1,2,3,4,5]  // []
// Start = combinations 5 [] // []
// Start = combinations 2 [1,2,3,4,5]  // [[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[2,5],[3,4],[3,5],[4,5]]
// Start = combinations 3 [1,2,3,4,5,6,7,8] // [[1,2,3,4,5,6],[1,2,3,4,5,7],[1,2,3,4,5,8],[1,2,3,4,6,7],[1,2,3,4,6,8],[1,2,3,4,7,8],[1,2,3,5,6,7],[1,2,3,5,6,8],
                                         // [1,2,3,5,7,8],[1,2,3,6,7,8],[1,2,4,5,6,7],[1,2,4,5,6,8],[1,2,4,5,7,8],[1,2,4,6,7,8],[1,2,5,6,7,8],[1,3,4,5,6,7],[1,3,4,5,6,8],[1,3,4,5,7,8],
                                         // [1,3,4,6,7,8],[1,3,5,6,7,8],[1,4,5,6,7,8],[2,3,4,5,6,7],[2,3,4,5,6,8],[2,3,4,5,7,8],[2,3,4,6,7,8],[2,3,5,6,7,8],[2,4,5,6,7,8],[3,4,5,6,7,8]]