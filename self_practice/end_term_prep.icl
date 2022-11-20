module end_term_prep
import StdEnv

 /*
    Returns the smallest element in a rose tree
 */
/*
:: RoseTree = Node Int [RoseTree]
r1 = Node 4 [Node 8 [], Node 12 [], Node 2 []]
r2 = Node 3 [r1, Node 5 []]
r3 = Node 20 [Node 0 [], Node 5 []]

minList :: [Int] -> Int
minList [x] = x
minList [x:xs] = min x (minList xs)

minRose :: RoseTree -> Int
minRose (Node x []) = x
minRose (Node x list) = min x (minList ([ minRose i \\ i<- list]))

//Start :: RoseTree
//Start = minRose r1 // 2
//Start = minRose r2 // 2
//Start = minRose r3 // 0
//Start =  minRose (Node 100 [r1, r2, r3]) // 0

*/
/*
8.Given binary search tree and Integer value, remove all the nodes from the tree which have this value.
Resulting tree should maintain binary search tree property.
*/

:: Tree a = Node a (Tree a) (Tree a) | Leaf

maxNode :: (Tree Int) -> (Int, Tree Int)
maxNode (Node x l Leaf) = (x, l)
maxNode (Node x l r) = (u, (Node x l mx))
where
   (u, mx) = maxNode r

joinNodes :: (Tree Int) (Tree Int) -> (Tree Int)
joinNodes Leaf nodeRight = nodeRight
joinNodes nodeLeft nodeRight = (Node u updatedLeft nodeRight)
where
    (u, updatedLeft) = (maxNode nodeLeft)

removeInt :: Int (Tree Int) -> (Tree Int)
removeInt _ Leaf = Leaf
removeInt u (Node x l r)
| u < x = Node x (removeInt u l) r
| u > x = Node x l (removeInt u r)
= joinNodes (removeInt u l) (removeInt u r)

//Start = removeInt 5 (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) // (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) Leaf)
//Start = removeInt 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 Leaf Leaf) Leaf) Leaf) Leaf) Leaf) Leaf) // Leaf
