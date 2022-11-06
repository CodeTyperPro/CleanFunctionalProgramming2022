module kthSymbol
import StdEnv

// On the first row, we write a 0. Now in every subsequent row, we look at the previous row and replace each occurrence of 0 with 01, and each occurrence of 1 with 10.
// Given row N and index K, return the K-th indexed symbol in row N. (The values of K are 1-indexed.) (1 indexed).

// findSymbol :: Int Int -> Int
// findSymbol n k
// | n == 1 = 0
// | k rem 2 == 1 = findSymbol (n-1) ((k+1)/2)
// = 1 bitxor (findSymbol (n-1) (k/2))

// Start = findSymbol 1 2
