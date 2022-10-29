module HW_6
import StdEnv

//Write your name and neptun code => HEIOPO | MARTINS Alfredo

/* Given the Major algebraic type of 4 majors and a Student record,
find the student with the highest average grade for each of the major and return their (Major, grade)
pair. The order of the resulting list is not important. */

:: Major = Finance | CS | Math | Physics
:: Student = {id :: Int, major::Major, grades::[Int]}

sts1 = {{id=1,major=Finance,grades=[5,5,5,5,5]},{id=2,major=CS,grades=[5,4,2,3,5,4,5]},{id=3,major=Finance,grades=[5,4,5,3,4,5]}, {id=4,major=Physics,grades=[2,3,4,2,5]}, {id=5,major=Math,grades=[3,4,5,3,2]}, {id=3,major=CS,grades=[5,4,4,4,4]}}
sts2 = {{id=0, major=Math, grades=[3,4,2,2]}}
sts3 = {}

getAverage :: [Int] -> Real
getAverage list = toReal(toReal(foldr (+) 0 list)/toReal(len))
where 
    len = length (list)

maxGrade :: (Real, Major) (Real, Major) -> (Real, Major)
maxGrade first second
| fst first > fst second = first
= second

maxAverage :: [(Real, Major)] -> (Real, Major)
maxAverage [x] = x
maxAverage [x : xs] = maxGrade x (maxAverage xs)

equal_major :: Major (Real, Major) -> Bool
equal_major major data = (snd data) == major

instance == Major 
where 
    == Finance Finance = True
    == CS CS = True
    == Math Math = True
    == Physics Physics = True
    == _ _ = False

highestGradesMajor :: [(Real, Major)] Major -> (Real, Major)
highestGradesMajor list major = maxAverage grades_filtered
where
    grades_filtered = filter (equal_major major) list

highestGrades :: {Student} -> [(Real, Major)]
highestGrades students = [ highestGradesMajor (AverageList) (item) \\ item <- ListMajors]
where
    ListMajors = removeDup [ item.major \\ item <-: students]
    AverageList = [ (getAverage item.grades, item.major) \\ item <-: students]

//Start = highestGrades sts1 // [(5, Finance), (4.2, CS), (3.2, Physics), (3.4, Math)]
//Start = highestGrades sts2 // [(2.75, Math)]
//Start = highestGrades sts3 // []

/* Given a (Tree Int), write a function which gives back a list that contains the values of the nodes that
has single subtree(which means either the right or the left child is a Leaf) */
:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf
          
Tree1 :: Tree Int
Tree1 = (Node 5 (Node 10 (Node 31 (Node 1 Leaf Leaf) Leaf) Leaf) (Node 17 (Node 31 (Node 14 (Node 12 Leaf Leaf) Leaf) Leaf) (Node 11 Leaf Leaf) ))

Tree2 :: Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)) 

Tree3 :: Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)

dephSubtree :: (Tree Int) -> Int
dephSubtree Leaf = 0
dephSubtree (Node _ left right) = (max (dephSubtree left) (dephSubtree right)) + 1

singleNodes :: (Tree Int) -> [Int]
singleNodes Leaf = []
singleNodes (Node x left right)
| dephSubtree left == 0 && dephSubtree right > 0 = [x] ++ singleNodes right
| dephSubtree right == 0 && dephSubtree left > 0 = [x] ++ singleNodes left
= (singleNodes left) ++ (singleNodes right)

//Start = singleNodes Tree1 // [10,31,1,31,14]
//Start = singleNodes Tree2 // []
//Start = singleNodes Tree3 // [1,3]