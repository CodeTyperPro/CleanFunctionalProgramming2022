module HW8
import StdEnv

// MARTINS Alfredo | HEIOPO

/* Given a binary search tree, change it into a sum tree.
Sum tree is a tree where each node is equal to its value + sum of all the nodes
that are greater in value. () -> represents the new value of the nodes

		 # Tree updated #

		        4 (30)
			   / \
			  /   \
			 /     \
		    /       \
		   /         \
		  /           \
		 /             \	 		
	    /               \				
  (36) 1                 6 (21)				
	  / \               / \ 				
(36) 0   2 (35)   (26) 5   7 (15)
          \				    \
           3 (33)            8 (8)

 */

:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = (Node 4 (Node 1 (Node 0 Leaf Leaf) (Node 2 Leaf (Node 3 Leaf Leaf)))(Node 6 (Node 5 Leaf Leaf) (Node 7 Leaf (Node 8 Leaf Leaf))))
tree2 = (Node 0 Leaf (Node 1 Leaf Leaf))
tree3 = (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)))

foldBST :: (Int Int -> Bool) Int (Tree Int) -> Int
foldBST function value Leaf = 0
foldBST function value (Node x left right)
| (function x value) = x + foldLeft + foldRight
= foldLeft + foldRight
where 
	foldLeft = (foldBST function value left)
	foldRight = (foldBST function value right)

mapBST :: (Int Int -> Bool) (Tree Int) (Tree Int)-> (Tree Int)
mapBST function Leaf originalTree = Leaf
mapBST function (Node x left right) originalTree = Node (x + mapNode) (mapLeft) (mapRight)
where 
	mapNode = foldBST (function) x originalTree
	mapLeft = mapBST (function) left originalTree
	mapRight = mapBST (function) right originalTree

BSTtoSumTree :: (Tree Int) -> (Tree Int)
BSTtoSumTree Leaf = Leaf
BSTtoSumTree originalTree = mapBST (>) originalTree originalTree

// Output updated, Node 1 should not exist.

//Start = BSTtoSumTree tree1 // (Node 30 (Node 36 (Node 36 Leaf Leaf) (Node 35 Leaf (Node 33 Leaf Leaf)))(Node 21 (Node 26 Leaf Leaf) (Node 15 Leaf (Node 8 Leaf Leaf))))
//Start = BSTtoSumTree tree2 // (Node 1 Leaf (Node 1 Leaf Leaf))
//Start = BSTtoSumTree tree3 // (Node 9 (Node 10 Leaf Leaf) (Node 7 Leaf (Node 4 Leaf Leaf)))


/* Word is type synonym of String.Define an operator <==>, and create an instance for Words which returns True 
if all the following conditions hold: 
	the number of consonants in both words are the same.
	the vowels in the words are the same.
	the number of upper and lower letter are equal.
*/

::Word :== String

//class <==> a
//instance <==> Word

numberOfConsonants :: String -> Int
numberOfConsonants string = length [c \\ c <-: string | not (isMember (toLower c) vowels) && isAlpha (toLower c)]

//Start = numberOfConsonants "HowAreYou-"

vowels = ['a', 'e', 'i', 'o', 'u']

vowelsOfTheWord :: String -> [Char]
vowelsOfTheWord string = sort [c \\ c <-: string | isMember (toLower c) vowels]

numberOfLowerCases :: String -> Int
numberOfLowerCases string = length [c \\ c <-: string | (c >= 'a' && c <= 'z')]

numberOfUpperCases :: String -> Int
numberOfUpperCases string = length [c \\ c <-: string | (c >= 'A' && c <= 'Z')]

Start = numberOfLowerCases "HowAreYou"

//Start = numberOfUpperCases "HowAreYou"
//Start = vowelsOfTheWord "HowAreYou?"
//Start = ["saah" <==> "sarah", "bOris" <==> "Boris", "functional" <==> "Functional", "abcde" <==> "abco", "haPPy" <==> "pLaYz"] // [False, True, False, False, True]
