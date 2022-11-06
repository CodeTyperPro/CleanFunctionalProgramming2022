module spiralMatrix
import StdEnv

// Iterate matrix in a spiral way
    // [1,2,3]
    // [8,9,4]
    // [7,6,5]

transpose :: [[Int]] -> [[Int]]
transpose [] = []
transpose m = [[(m!!j)!!i \\ j<-[0..((length m)-1)]] \\ i<-[0..(length (m!!0) - 1)]]



matrix = [
    [1,2,3],
    [4,5,6],
    [7,8,9]]
spiral :: [[Int]] -> [Int]
spiral [x:xs] = x ++ (spiral (reverse (transpose(xs))))
spiral _ = []

// Start = spiral matrix