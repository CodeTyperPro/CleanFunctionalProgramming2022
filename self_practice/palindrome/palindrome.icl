module palindrome
import StdEnv

split_number :: Int ->  [Int]
split_number n
| n < 10 = [n]
= [(n rem 10)] ++ split_number (n/10)

power :: Int Int -> Int
power x p
| p == 0 = 1
= x*power x (p-1)

convert_to_list :: Int -> [Int]
convert_to_list n = split_number n

convert_to_number :: [Int] -> Int
convert_to_number n
| length n == 0 = 0
= (n!!0)*power 10 (length n) + convert_to_number (tl n)

check_palindrome :: Int -> Bool
check_palindrome x = x == (convert_to_number (convert_to_list x))/10

Start = check_palindrome 12621