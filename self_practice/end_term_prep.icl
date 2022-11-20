module end_term_prep
import end_term_prep

 /*
    Returns the smallest element in a rose tree
 */

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