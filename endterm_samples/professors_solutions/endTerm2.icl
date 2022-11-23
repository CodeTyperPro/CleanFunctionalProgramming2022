module endTerm2

import StdEnv 


/*
1. Given two integer numbers k and n.
Generate a String by the following pattern:
In case k=2 and n=3
"[[**[[**[[**"
So n times, k opening brackets followed by k stars.
*/

bracketsStars::Int Int -> String
bracketsStars k n = {c \\c <- x}
where
	x = flatten [['[' \\ a<-[1..k]] ++['*' \\ a<-[1..k]] \\ b<-[1..n]]
	
Start = bracketsStars 2 3 // "[[**[[**[[**"
//Start = bracketsStars 0 3 // ""
//Start = bracketsStars 5 2 // "[[[[[*****[[[[[*****"


/*
2. Write a function which takes a String and modifies it such that
only the first character is uppercase and every other one is lowercase.
The given string contains only letters!

Hints:
toInt('a') - toInt('A') == 32
Uppercase letters are in the range [65,90] (ASCII)
Lowercase letters are in the range [97,122] (ASCII)

Example:
"abcdFeH" = "Abcdfeh"
*/

tolist :: String -> [Char]
tolist str = [x \\ x <-: str ]

help :: [Char] -> String 
help [] = ""
help [x:xs] 
|toInt x >= 65 && toInt x <= 90 =  toString (toChar (toInt x + 32 )) +++  help xs 
= toString x +++ help xs

capitalFirst :: [Char] -> String 
capitalFirst [] = ""
capitalFirst [x:xs] = toString (toChar (toInt x - 32 )) +++ toStr xs

toStr :: [Char] -> String 
toStr [] = ""
toStr [x:xs] = toString x +++ toStr xs

capitalize::String -> String
capitalize str = capitalFirst (tolist (help (tolist str))) 

//Start = capitalize "aABCDEFG" // "Aabcdefg"
//Start = capitalize "abABab" // "Ababab"
//Start = capitalize "" // ""


:: Major = Finance | CS | Math | Physics | Economy | Linguistics
:: Course = {name::String, major::Major, credits::Int}

OOP::Course
OOP = {name="OOP",major=CS, credits=5}
Discrete_math::Course
Discrete_math = {name="Discrete_math",major=Math, credits=4}
Relativity::Course
Relativity = {name="Relativity", major=Physics, credits=6}
Functional::Course
Functional = {name="Functional", major=CS, credits=5}
Quantum_mechanics::Course
Quantum_mechanics = {name="Quantum_mechanics", major=Physics, credits=4}
Corporate_finance::Course
Corporate_finance = {name="Corporate_finance", major=Finance, credits=6}
Venture_captical::Course
Venture_captical = {name="Venture_captical", major=Finance, credits=6}
Macroeconomics::Course
Macroeconomics = {name="Macroeconomics", major=Economy, credits=6}
Microeconomics::Course
Microeconomics = {name="Microeconomics", major=Economy, credits=6}
Numerical_Methods::Course
Numerical_Methods = {name="Numerical_Methods", major=Math, credits=4}
Cryptography::Course
Cryptography = {name="Cryptography", major=CS, credits=2}
Phonology::Course
Phonology = {name="Phonology", major=Linguistics, credits=3}
Morphology::Course
Morphology = {name="Morphology", major=Linguistics, credits=3}


/*
3. Given a list of Courses and a major, check if any of the courses in the list
has the same major with the given one, return True if there is at least one.
*/

instance == Major 
where
	(==) Finance Finance = True
	(==) CS CS = True
	(==) Math Math = True
	(==) Physics Physics = True
	(==) Economy Economy = True
	(==) Linguistics Linguistics = True
	(==) _ _ = False
	
same_major :: [Course] Major -> Bool
same_major list m = or [m == a.major \\ a <- list]

//Start = same_major [Corporate_finance, OOP, Microeconomics] Finance // True
//Start = same_major [Morphology, Macroeconomics, Quantum_mechanics] CS // False
//Start = same_major [Venture_captical, Relativity, Cryptography] Physics // True
//Start = same_major [Discrete_math] Economy // False
//Start = same_major [] Physics // False


/*
4. Given a list of Courses that a student has taken, write a function that
returns a list of records (you need to define by yourself, called Earned_credits),
each records has the name of the major (field) and
the credits the student earned for each major (earned).
*/

:: Earned_credits = {major1 :: Major, credits1 :: Int}

upd :: [Earned_credits] Course -> [Earned_credits]
upd [] y = [{major1=y.major, credits1 = y.credits}]
upd [x:xs] y
|x.major1 == y.major = [{x & credits1 = x.credits1 + y.credits} : xs]
= [x : upd xs y]  

calc :: [Course] -> [Earned_credits]
calc [] = []
calc [x:xs] = upd (calc xs) x

//Start = calc [Functional, OOP, Relativity] // [(Earned_credits CS 10),(Earned_credits Physics 6)]
//Start = calc [Morphology, Macroeconomics, Numerical_Methods] // [(Earned_credits Math 4),(Earned_credits Economy 6),(Earned_credits Linguistics 3)]
//Start = calc [Corporate_finance, Numerical_Methods, Cryptography] // [(Earned_credits Finance 6),(Earned_credits Math 4),(Earned_credits CS 2)]
//Start = calc [] // []


/*
5. Write '+' operator for lists.
If both lists are sorted in increasing order you should merge them
in a way that resulting list is sorted too.
Ex.: [1,3,6] + [2,4,5,7] -> [1,2,3,4,5,6,7]
If list is not sorted than it is considered empty.
Ex.: [1,3,6] + [2,3,1] -> [1,3,6] + [] -> [1,3,6]
Ex.: [2,9,7] + [5,4,3] -> [] + [] -> []
*/

checkL :: [a] -> Bool | Eq, Ord a
checkL l = l == sort l

do :: [a] [a] -> [a] | Eq, Ord a
do l1 l2
| checkL l1 && checkL l2 = merge l1 l2                                             			
| checkL l1 = l1
| checkL l2 = l2
= []   

instance + [a] | Eq, Ord a
where
	(+) l1 l2 = do l1 l2
	
//Start = [1,2,3] + [1,3,6] // [1,1,2,3,3,6]
//Start = [1,3,6] + [2,4,5,7] // [1,2,3,4,5,6,7]
//Start = [1,3,6] + [2,3,1] // [1,3,6]
//Start = [5,1] + [1,3,6] // [1,3,6]
//Start = [] + [1] // [1]
//Start = [2,3,1] + [12,3,1] // []
//Start :: [Int]
//Start = [] + [] // []


:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 :: Tree Int
tree1 = (Node 4 (Node 10 (Node 6 Leaf Leaf)(Node 11 Leaf Leaf)) (Node 20 (Node 12 Leaf Leaf) Leaf))
tree2 :: Tree Int
tree2 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))
tree3 :: Tree Int
tree3 = (Node 12 (Node 11 (Node 11 (Node 32 Leaf Leaf) Leaf) Leaf) (Node 4 (Node 17 (Node 5 (Node 7 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf (Node 4 Leaf Leaf)) ))
tree4 :: Tree Int
tree4 = (Node 7 (Node 11 tree1 tree2) (Node 5 tree3 tree2))
tree5 :: Tree Int
tree5 = Node 1 tree3 tree4


/*
6. Define == for Tree. Two Trees are equal if each node value
from 1st tree is equal to 2nd tree.
*/

nodes :: (Tree a) -> [a] 
nodes Leaf = [] 
nodes (Node a l r) = nodes l ++ [a] ++ nodes r 

instance == (Tree a) | == a
where 
	(==) t1 t2 = (nodes t1) == (nodes t2)

//Start = tree1 == tree1 // True
//Start = tree2 == tree3 // False
//Start = tree4 == tree4 // True
//Start = tree1 == tree5 // False


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


/*
7. Having a list of students, each student has a list of grades
create a balanced (or almost balanced) binary search tree based on the students' grades average.
Info: balance tree means that the difference between the depth of the left tree and the depth of 
the right tree is less than 1
Note: There is no dublication in the averages.
*/

avgGr :: [Int] -> Real
avgGr list = (toReal (sum list)) /(toReal (length list))

sortStud :: [Student] -> [Student] 
sortStud [] = []
sortStud [x:xs] = sortStud [a \\ a<-xs | (avgGr a.grades) < (avgGr x.grades)] ++ [x] ++ sortStud [a \\ a<-xs | (avgGr a.grades) >= (avgGr x.grades)]

createStudTree :: [Student] -> (Tree Student)
createStudTree [] = Leaf
createStudTree list = Node (sortedSt!!half) (createStudTree (take half sortedSt)) (createStudTree (drop (half+1) sortedSt))  
where
	sortedSt = sortStud list
	half = (length list)/2
  
//Start = createStudTree [] // Leaf
//Start = createStudTree [A,B,C] //(Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf))
//Start = createStudTree [A,B,C,D] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [C,D,A,B] //(Node (Student "C" [1,2,3,4,5]) (Node (Student "B" [1,2,3,4]) (Node (Student "A" [1,2,3]) Leaf Leaf) Leaf) (Node (Student "D" [1,2,3,4,5,6]) Leaf Leaf))
//Start = createStudTree [F,E,C,D,A,B] //(Node (Student "B" [1,2,3,4]) (Node (Student "F" [1,2]) (Node (Student "E" [1]) Leaf Leaf) (Node (Student "A" [1,2,3]) Leaf Leaf))
// (Node (Student "D" [1,2,3,4,5,6]) (Node (Student "C" [1,2,3,4,5]) Leaf Leaf) Leaf))


/*
8.Given binary search tree and Integer value, remove all the nodes from the tree which have this value.
Resulting tree should maintain binary search tree property.
*/

removeInt :: Int (Tree Int) -> (Tree Int)
removeInt y Leaf = Leaf
removeInt y (Node x l r)
| y==x = Leaf
= (Node x (removeInt y l) (removeInt y r))

//Start = removeInt 5 (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) (Node 5 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) // (Node 4 (Node 3 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 4 Leaf Leaf)) Leaf)
//Start = removeInt 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 (Node 1 Leaf Leaf) Leaf) Leaf) Leaf) Leaf) Leaf) // Leaf


/*
9.Complementary colors are pairs of colors which, when combined or mixed,
cancel each other out (lose hue) by producing a grayscale color like white or black.
Such pairs are:
Red - Green
Orange - Blue
Yellow - Purple
Violet - Amber
Teal - Vermilion
Magenta - Chartreuse
Create an instance == for the Color and
write a function that finds complement of a given color.
*/

:: Color = Red | Yellow | Green | Blue | Purple | Orange | Violet | Amber | Teal | Vermilion | Magenta | Chartreuse

colorList = [Red, Yellow, Green, Blue, Purple, Orange, Violet, Amber, Teal, Vermilion, Magenta, Chartreuse]

instance == Color
where
	(==) Red Green = True
	(==) Green Red = True
	(==) Yellow Purple = True 
	(==) Purple Yellow = True
	(==) Blue Orange = True
	(==) Orange Blue  = True
	(==) Violet Amber = True 
	(==) Amber Violet = True
	(==) Vermilion Teal = True
	(==) Teal  Vermilion = True
	(==) Magenta Chartreuse = True 
	(==) Chartreuse Magenta = True
	(==) _ _ = False
	
find_complement :: Color -> Color
find_complement c = hd [ x \\ x<-colorList | c == x]

//Start = find_complement Red // Green
//Start = find_complement Green // Red
//Start = find_complement Teal // Vermilion
//Start = find_complement Chartreuse // Magenta
//Start = find_complement Violet // Amber


/*
10.Create a class called Comparisons and define the binary operations:
*== , != , *< , *> , *<= ,*>=
Given two elements it compares them and gives out a boolean.
Example: x *== y should check if the x and y are equal.
!= -> not equal
*< -> less (smaller)
*> -> greater (bigger)
*<= -> less (smaller) or equal
*>= -> greater (bigger) or equal
Make an instance for Vector3 type. 
instance Comparisons Vector3
To compare two vectors use their length.
Length of 3 dimensional vector (a,b,c) is sqrt(a^2 + b^2 + c^2)
*/

::Vector3 = { x :: Real, y :: Real, z :: Real}

class Comparisons a
where 
	 (*==) :: a a -> Bool
	 (*<) :: a a -> Bool
	 (*>) :: a a -> Bool
     (*<=) :: a a -> Bool
     (*>=) :: a a -> Bool
     (!=) :: a a -> Bool
     
instance Comparisons Vector3
	where  
		(*==) :: Vector3 Vector3 -> Bool 
		(*==) v1 v2 = vlen v1 ==  vlen v2
		(*<) :: Vector3 Vector3 -> Bool 
		(*<) v1 v2 = vlen v1 < vlen v2
		(*>) :: Vector3 Vector3 -> Bool 
		(*>) v1 v2 = vlen v1 > vlen v2
		(*>=) :: Vector3 Vector3 -> Bool 
		(*>=) v1 v2 = vlen v1 >= vlen v2
	    (*<=) :: Vector3 Vector3 -> Bool 
		(*<=) v1 v2 = vlen v1 <= vlen v2
	    (!=) :: Vector3 Vector3 -> Bool 
		(!=) v1 v2 = vlen v1 <> vlen v2
		
vlen :: Vector3 -> Real 		  
vlen v = sqrt( v.x*v.x + v.y*v.y + v.z*v.z ) 
 
//Start = {x = 1.0, y = 1.0, z = 1.0} *== {x = 1.0, y = 1.0, z = 1.0} // True
//Start = {x = 3.0, y = 4.0, z = 10.0} *== {x = 5.0, y = 0.0, z = 10.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} != {x = 2.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *< {x = 2.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *> {x = 2.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *<= {x = 1.0, y = 1.0, z = 1.0} // True
//Start = {x = 1.0, y = 1.0, z = 1.0} *>= {x = 1.0, y = 1.0, z = 1.0} // True

 

 

