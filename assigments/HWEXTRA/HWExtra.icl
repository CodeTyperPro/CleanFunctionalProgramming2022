module HWExtra
import StdEnv

:: Bank = AMEX | VISA | MASTERCARD

:: Account = {name :: String, issuer :: Bank, cardnumber :: Int, age :: Int}

/* There has been news of a fraud, where invalid credit cards are being used without being noticed.
Now the World bank would like to check the validity of the credit cards numbers' of every major bank.
Therefore make a function that will return invalid account's (name, issuer) pairs from the array of accounts. 

Validation: 
1. check prefix and length
	AMEX: start with 34 or 37 and has 15 numbers.
	MasterCard: start with 51, 52, 53, 54, or 55 and has 16 numbers.
	Visa: start with 4539, 4556, 4916, 4532, 4929, 40240071, 4485, 4716 or 4 and has 13 or 16 numbers.
2. implement luhn algorithm (https://en.wikipedia.org/wiki/Luhn_algorithm
							https://www.groundlabs.com/blog/anatomy-of-a-credit-card/)
*/

toListDigit :: Int -> [Int]
toListDigit x 
| x < 10 = [x]
= (toListDigit (x/10)) ++ [x rem 10]

//Start = toListDigit 1234

compute :: Int -> Int
compute x 
| x >= 10 = compute ((x/10) + (x rem 10))
= x

//Start = compute 19

Luhn_algorithm :: Int Int -> Int
Luhn_algorithm x d
| x < 10 = compute u
= (compute u) + (Luhn_algorithm (x/10) ((d rem 2) + 1))
where
	u = (x rem 10)*d

//Start = Luhn_algorithm 3379513561108795 1 // 70, start in 2
//Start = Luhn_algorithm 7992739871 2 // 67, start in 1

isValidCardNumber :: Int -> Bool
isValidCardNumber x = ((Luhn_algorithm x 1) rem 10 == 0) || ((Luhn_algorithm x 2) rem 10 == 0)

//Start = isValidCardNumber 3379513561108795 // True
//Start = isValidCardNumber 7992739871 // False
//Start = isValidCardNumber 79927398713 // True

// --- Checking --- //
prefix_len_AMEX = ([34, 37], [15])
prefix_len_MASTERCARD = ([51, 52, 53, 54, 55], [16])
prefix_len_VISA = ([4539, 4556, 4916, 4532, 4929, 40240071, 4485, 4716, 4], [13, 16])

checkPrefixBank :: Bank Int -> Bool
checkPrefixBank AMEX x = isMember (x/(10^(15 - 2))) (fst prefix_len_AMEX)
checkPrefixBank MASTERCARD x = isMember (x/(10^(16 - 2))) (fst prefix_len_MASTERCARD)
checkPrefixBank VISA x = u || y || z || w
where
	u = isMember (x/(10^(13 - 4))) (fst prefix_len_VISA)
	y = isMember (x/(10^(16 - 4))) (fst prefix_len_VISA)
	w = isMember (x/(10^(13 - 1))) (fst prefix_len_VISA)
	z = isMember (x/(10^(16 - 1))) (fst prefix_len_VISA)

//Start = (4111111111111111/(10^(15)))

/* For VISA, all the given code start with 4 and it the card can start exactly with 4, then, checking start with 4, fulfill all the conditions ... */

//Start = checkPrefixBank VISA 4155279860457 // False
//Start = checkPrefixBank VISA 6155279860457 // False

//Start = checkPrefixBank VISA 4111111111111111 // True
//Start = checkPrefixBank VISA 4222222222222 // True

//Start = checkPrefixBank VISA 4263982640269299 // True
//Start = checkPrefixBank VISA 4917484589897107 // True	

//Start = checkPrefixBank VISA 4001919257537193 // True

//Start = checkPrefixBank MASTERCARD 5555555555554444 // True
//Start = checkPrefixBank MASTERCARD 5105105105105100 // True

//Start = checkPrefixBank MASTERCARD 5425233430109903 // True
//Start = checkPrefixBank MASTERCARD 5425233430109903 // True

//Start = checkPrefixBank MASTERCARD 2222420000001113 // False
//Start = checkPrefixBank MASTERCARD 2223000048410010 // False

//Start = checkPrefixBank AMEX 378282246310005 // True
//Start = checkPrefixBank AMEX 371449635398431 // True

//Start = checkPrefixBank AMEX 378734493671000 // True
//Start = checkPrefixBank AMEX 374245455400126 // True
//Start = checkPrefixBank AMEX 378282246310005 // True

checkPrefix :: Bank Int -> Bool
checkPrefix bank x = (checkPrefixBank bank x)

getLen :: Bank -> [Int]
getLen AMEX = snd prefix_len_AMEX
getLen MASTERCARD = snd prefix_len_MASTERCARD
getLen VISA = snd prefix_len_VISA
getLen _ = []

checkLength :: Bank Int -> Bool
checkLength bank x = isMember len (getLen bank)
where
	len = length (toListDigit x)

checkAccount :: Account -> Bool
checkAccount account = (checkPrefix v u) && (checkLength v u) && u > 0
where
	u = account.cardnumber
	v = account.issuer

invalid :: {Account} -> {(String, Bank)}
invalid array = { (i.name, i.issuer)  \\ i <-: array | not (checkAccount i)}

/*Start = invalid {{name = "Sheldon", issuer = AMEX, cardnumber = 374245455400126, age = 30}, 
					{name = "Raj", issuer = AMEX, cardnumber = 378282246319905, age = 31}, 
					{name = "Leonard", issuer = MASTERCARD, cardnumber = 378282246310005, age = 33}, 
					{name = "Penny", issuer = MASTERCARD, cardnumber = 552523340109903, age = 32}, 
					{name = "Amy", issuer = VISA, cardnumber = 4917484589897107, age = 31}, 
					{name = "Howard", issuer = VISA, cardnumber = 79927398713, age = 30}
				   }*/
// {("Raj", AMEX), ("Penny", MASTERCARD) ("Howard", VISA)}
/**/
// Raj's card is valid AMEX. ("Raj", AMEX) ??? => ("Leonard",MASTERCARD)

:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 1 (Node 2 (Node 4 Leaf Leaf)(Node 5 Leaf Leaf)) (Node 1 (Node 6 Leaf Leaf) Leaf))

tree2 :: Tree Int
tree2 = (Node 4 (Node 3 (Node 11 (Node 2 Leaf Leaf) Leaf) Leaf) (Node 0 (Node 2 (Node 13 (Node 15 Leaf Leaf) Leaf) Leaf) (Node 1 Leaf Leaf) ))

tree3 :: Tree Int
tree3 = (Node -1 (Node -3 (Node -11 (Node -2 Leaf Leaf) Leaf) Leaf) (Node -16 (Node -22 (Node -13 (Node -15 Leaf Leaf) Leaf) Leaf) (Node -100 Leaf (Node -1 Leaf Leaf)) ))


/* Given a binary tree return its level order traversal 

e.g 3 -> [3]
   / \
   9  20 -> [9,20]
  /   / \
  1  15 7 -> [1,15,7] => [[3], [9,20],[1,15,7]] */

TreeTraversal :: (Tree Int) Int -> [(Int, Int)]
TreeTraversal Leaf _ = []
TreeTraversal (Node x l r) c = [(x, c)] ++ (TreeTraversal l u) ++  (TreeTraversal r u)
where
	u = c + 1

levelTree :: (Tree Int) -> Int
levelTree Leaf = 0
levelTree (Node x l r) = 1 + max (levelTree l) (levelTree r)

//Start = levelTree tree1

levelOrder :: (Tree Int) -> [[Int]]
levelOrder Leaf = []
levelOrder tree = [[fst u \\ u<- list | (snd u) == i] \\ i <- [1..level]]
where
	list = (TreeTraversal tree 1)
	level = levelTree tree

//Start = levelOrder tree1 // [[1],[2,1],[4,5,6]]
//Start = levelOrder tree2 // [[4],[3,0],[11,2,1],[2,13],[15]]
//Start = levelOrder tree3 // [[-1],[-3,-16],[-11,-22,-100],[-2,-13,-1],[-15]]
//Start = levelOrder  (Node 3 (Node 9 (Node 1 Leaf Leaf) Leaf) (Node 20 (Node 15 Leaf Leaf) (Node 7 Leaf Leaf))) // [[3],[9,20],[1,15,7]]