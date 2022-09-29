module factors
import StdEnv

fact :: Int -> [Int]
fact n = [x \\ x <- [1..n] | n rem x == 0]

Start = fact 10