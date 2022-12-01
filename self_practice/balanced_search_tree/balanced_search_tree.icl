module balanced_search_tree
import StdEnv

/*
7. Having a list of students, each student has a list of grades
create a balanced (or almost balanced) binary search tree based on the students' grades average.

Info: balance tree means that the difference between the depth of the left tree and the depth of 
the right tree is less than 1
Note: There is no duplication in the averages.
*/


:: Student = {s_name::String, grades::[Int]}

A :: Student
A = {s_name="A", grades = [1,2,3]}
B :: Student
B = {s_name="B", grades = [1,2,3,4]}
C :: Student
C = {s_name="C", grades = [1,2,3,4,5]}
D :: Student
D = {s_name="D", grades = [1,2,3,4,5,6]}
E :: Student
E = {s_name="E", grades = [1]}
F :: Student
F = {s_name="F", grades = [1,2]}

avg :: [Int] -> Real
avg list =  toReal ((sum (list))/(length list))

:: Tree a = Node a (Tree a) (Tree a) | Leaf

createStudTree :: [Student] -> (Tree Student)
createStudTree [] = Leaf
createStudTree students = Node x left right
where
	list = sortBy (\ x y = (avg x.grades) < (avg y.grades)) students
	len = (length students) / 2
	x = list !! len
	left = createStudTree (take len list)
	right = createStudTree (drop (len + 1) list)

//Start = createStudTree [] // Leaf
//Start = createStudTree [A,B,C] //(Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf))
//Start = createStudTree [A,B,C,D] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [C,D,A,B] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [F,E,C,D,A,B] //(Node (Student "B" [1,2,3,4]) (Node (Student "F" [1,2]) (Node (Student "E" [1]) Leaf Leaf) (Node (Student "A" [1,2,3]) Leaf Leaf))
// (Node (Student "D" [1,2,3,4,5,6]) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf) Leaf))
