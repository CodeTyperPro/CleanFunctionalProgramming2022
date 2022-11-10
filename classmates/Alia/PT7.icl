module PT7
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf
          
/*
	Given a tree and check if each node has even left child and odd right child.
					 10
				   /    \
				  2      3
				 / \    / \
				4   7  6   5
			   / \ / \/ \ / \
			  8   LL LL LL   1
			  
			  That tree given returns TRUE because each node has even left child and odd right child
	
*/

treeOne = Node 10 (Node 2 (Node 4 (Node 8 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 1 Leaf Leaf)))
treeTwo = Node 10 (Node 2 (Node 4 (Node 9 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 1 Leaf Leaf))) 
treeThree = Node 10 (Node 2 (Node 4 (Node 8 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 2 Leaf Leaf))) 


instance == (Tree a) | == a
where
    (==) Leaf Leaf = True
    (==) (Node x1 l1 r1) (Node x2 l2 r2) = and[x1==x2, l1==l2, r1==r2]
    (==) _ _ = False

getKey :: (Tree Int) -> Int
getKey Leaf = abort "Invalid"
getKey (Node x l r) = x

PT7 :: (Tree Int) -> Bool
PT7 Leaf = True
PT7 (Node x Leaf Leaf) = True 
PT7 (Node x Leaf r) = (isOdd (getKey r)) && (PT7 r)
PT7 (Node x l Leaf) = (isEven ( getKey l)) && (PT7 l)
PT7 (Node x l r) = (isEven (getKey l)) && (isOdd (getKey r)) && (PT7 l) && (PT7 r)

//Start = PT7 treeOne	// True
//Start = PT7 treeTwo 	// False
//Start = PT7 treeThree 	// False

