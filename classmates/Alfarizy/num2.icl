module num2 
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

:: University = ELTE | BME | Corvinus
:: Student = {name::String, uni :: University, grades:: [Int]}

Rose::Student
Rose = {name="Rose",uni=ELTE, grades =[5,5,3,4,2,4,5,5]}


Peter::Student
Peter = {name="Peter",uni=BME, grades =[3,2,3,4,2,4,2,1,4,3,2,4]}


Noah::Student
Noah = {name="Noah",uni=Corvinus,grades=[1,2,2,3,1,3,4,2,3,4,2,4,2,1]}


James::Student
James = {name="James",uni=ELTE,grades=[5,5,5,5,3,4,5,4,5]}


Lily::Student
Lily = {name="Lily",uni=BME,grades=[1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]}


Harry::Student
Harry = {name="Harry",uni=Corvinus,grades=[3,4,1,3,4,2,3,5,5]}


Eros::Student
Eros = {name="Eros",uni=Corvinus,grades=[4,2,4,4,4,4,4,5,2]}


Isabella::Student
Isabella = {name="Isabella",uni=BME,grades=[5,5,5,4,5,5,4,5,4,5]}


Oliver::Student
Oliver = {name="Oliver",uni=ELTE,grades=[2,3,3,4,3,2,1,3,2,3]}

tree1 :: (Tree Student)
tree1 = (Node Eros (Node Oliver Leaf Leaf) (Node Harry Leaf (Node Noah Leaf Leaf)))

tree2 :: (Tree Student)
tree2 = (Node Rose (Node James Leaf Leaf) (Node James Leaf (Node Oliver (Node Rose Leaf Leaf) (Node James Leaf Leaf))))

tree3 :: (Tree Student)
tree3 = (Node Oliver (Node Rose tree2 tree2) (Node James tree2 (Node Oliver tree2 tree2)))

/*2
 Given a binary tree of Students and a University, find the size of the largest subtree, where
 every student is from the given University.
 
 A subtree of a binary tree is a tree that consists of
 a node in the tree and all of this node's descendants.
 NOTE: The largest tree is the tree with the biggest number of nodes
*/

instance == University
where    
      (==) ELTE ELTE = True
      (==) BME BME = True
      (==) Corvinus Corvinus = True
      (==) _ _ = False

sizeTree :: (Tree Student) University -> Int
sizeTree Leaf uni = 0
sizeTree (Node x l r) uni
| x.uni == uni = 1 + (sizeTree l uni) + (sizeTree r uni)
= (sizeTree l uni) + (sizeTree r uni)

largestUniversitySubtree :: University (Tree Student) -> Int
largestUniversitySubtree uni Leaf = 0
largestUniversitySubtree uni (Node x l r)
| uni == x.uni = max size (max (largestUniversitySubtree uni l) (largestUniversitySubtree uni r))
= max (largestUniversitySubtree uni l) (largestUniversitySubtree uni r)
where
	size = 1 + (sizeTree l uni) + (sizeTree r uni)

//Start = largestUniversitySubtree ELTE Leaf // 0
//Start = largestUniversitySubtree Corvinus tree1 // 2
//Start = largestUniversitySubtree ELTE tree1 // 1
//Start = largestUniversitySubtree ELTE tree3 // 34
//Start = largestUniversitySubtree BME tree3 // 0