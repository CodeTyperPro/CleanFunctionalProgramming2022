module HW8
import StdEnv

/* Given a binary search tree, change it into a sum tree.
Sum tree is a tree where each node is equal to its value + sum of all the nodes
that are greater in value. () -> represents the new value of the nodes

		4(30)				
	   / \				
 (36) 1   6	(21)				
	 / \ / \ 				
(36)0  2 5  7 (15)
       \ (26)\
   (33) 3     8 (8)
 */

:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = (Node 4 (Node 1 (Node 0 Leaf Leaf) (Node 2 Leaf (Node 3 Leaf Leaf)))(Node 6 (Node 5 Leaf Leaf) (Node 7 Leaf (Node 8 Leaf Leaf))))
tree2 = (Node 0 Leaf (Node 1 Leaf Leaf))
tree3 = (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)))

BSTtoSumTree :: (Tree Int) -> Int

//Start = countPalindromic tree1 // (Node 30 (Node 1 (Node 36 Leaf Leaf) (Node 35 Leaf (Node 33 Leaf Leaf)))(Node 21 (Node 26 Leaf Leaf) (Node 15 Leaf (Node 8 Leaf Leaf))))
//Start = countPalindromic tree2 // (Node 1 Leaf (Node 1 Leaf Leaf))
//Start = countPalindromic tree3 // (Node 9 (Node 10 Leaf Leaf) (Node 7 Leaf (Node 4 Leaf Leaf)))

/* Word is type synonym of String.Define an operator <==>, and create an instance for Words which returns True 
if all the following conditions hold: 
	the number of consonants in both words are the same.
	the vowels in the words are the same.
	the number of upper and lower letter are equal.
*/

::Word :== String

//class <==> a
//instance <==> Word

//Start = ["saah" <==> "sarah", "bOris" <==> "Boris", "functional" <==> "Functional", "abcde" <==> "abco", "haPPy" <==> "pLaYz"] // [False, True, False, False, True]
