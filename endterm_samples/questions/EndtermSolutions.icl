module EndtermSolutions
import StdEnv

:: Tree a = Node a (Tree a) (Tree a)
			| Leaf
:: Month = January | February  | March   | April    | May | June | July
         | August  | September | October | November | December

bestTree = Node 10(Node 6(Node 1 Leaf(Node 5(Node 2 Leaf(Node 4(Node 3 Leaf Leaf)Leaf))Leaf))Leaf)(Node 14(Node 11 Leaf(Node 13(Node 12 Leaf Leaf)Leaf))(Node 17(Node 15 Leaf(Node 16 Leaf Leaf))(Node 19(Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))		
noTree = Leaf
unitTree = Node 1337 Leaf Leaf

extractNode :: (Tree Int) -> Int
extractNode (Node x l r) = x

goL :: (Tree Int) -> (Tree Int)
goL (Node x l r) = l
goR :: (Tree Int) -> (Tree Int)
goR (Node x l r) = r
isLeaf :: (Tree Int) -> Bool
isLeaf Leaf = True
isLeaf _ = False

//Write a function that takes a tree as a parameter and returns a list of leaves.
// An empty tree will return [] and a single element tree will return a list of one element.
leaves :: (Tree Int) -> [Int]
leaves Leaf = []
leaves (Node x l r)
| isLeaf l && isLeaf r = [x]
= (leaves l)++(leaves r)

//Start = leaves bestTree //[3,12,16,18,20]
//Start = leaves ourTree //[1,8,11,19,24,28]
//Start = leaves unitTree //[1337]
//Start =  leaves noTree //[]

minTree :: (Tree Int) -> Int
minTree tree
| isLeaf(goL tree)= extractNode tree
= minTree (goL tree)

//Remove the minimum node of a binary tree
remMin :: (Tree Int) -> (Tree Int)
remMin (Node x Leaf r) = r
remMin (Node x l r)
| extractNode l == minTree (Node x l r) = (Node x (goR l) r)
= (Node x (remMin l) r)

//Write a function that takes a binary search tree, and removes its node.
//Note, the original tree connections must be preserved.
remRoot :: (Tree Int) -> (Tree Int)
remRoot Leaf = Leaf
remRoot (Node x l r)
| isLeaf l && isLeaf r = Leaf
| isLeaf l = r
| isLeaf r = l
= (Node (minTree r) l (remMin r))


//Start = remRoot ourTree //(Node 18 (Node 3 (Node 1 Leaf Leaf) (Node 10 (Node 7 Leaf (Node 8 Leaf Leaf)) (Node 13 (Node 11 Leaf Leaf) Leaf))) (Node 20 (Node 19 Leaf Leaf) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf)))))
//Start = remRoot bestTree //(Node 11 (Node 6 (Node 1 Leaf (Node 5 (Node 2 Leaf (Node 4 (Node 3 Leaf Leaf) Leaf)) Leaf)) Leaf) (Node 14 (Node 13 (Node 12 Leaf Leaf) Leaf) (Node 17 (Node 15 Leaf (Node 16 Leaf Leaf)) (Node 19 (Node 18 Leaf Leaf) (Node 20 Leaf Leaf)))))
//Start = remRoot unitTree //Leaf
//Start = remRoot noTree //Leaf

isPrime :: Int -> Bool
isPrime n = isEmpty[x\\x<-[2..(n-1)]|n rem x == 0]

//Write a function that takes a condition testing function (such as isPrime, isEven, etc)
//and a binary search tree, and filters the tree according to the condition.
//That is, you must remove the nodes that do not meet the condition and keep those that do
//while preserving the original tree connections.
filterTree :: (Int -> Bool) (Tree Int) -> (Tree Int)
filterTree cond Leaf = Leaf
filterTree cond (Node x l r)
| cond x = (Node x (filterTree cond l) (filterTree cond r))
= remRoot (Node x (filterTree cond l) (filterTree cond r))

//Start =  filterTree isEven bestTree //(Node 10 (Node 6 (Node 2 Leaf (Node 4 Leaf Leaf)) Leaf) (Node 14 (Node 12 Leaf Leaf) (Node 18 (Node 16 Leaf Leaf) (Node 20 Leaf Leaf))))
//Start = filterTree isEven ourTree //(Node 18 (Node 10 (Node 8 Leaf Leaf) Leaf) (Node 20 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
//Start = filterTree ((>)10) unitTree //Leaf
//Start = filterTree isOdd noTree //Leaf

//Write a function that takes a month and a natural number
//then returns that number of months later.
//For example, monthIterate January 10 = November
monthIterate :: Month Int -> Month
monthIterate January 1 = February
monthIterate January n = monthIterate February ((n rem 12)-1)
monthIterate February 1 = March
monthIterate February n = monthIterate March ((n rem 12)-1)
monthIterate March 1 = April
monthIterate March n = monthIterate April ((n rem 12)-1)
monthIterate April 1 = May
monthIterate April n = monthIterate May ((n rem 12)-1)
monthIterate May 1 = June
monthIterate May n = monthIterate June ((n rem 12)-1)
monthIterate June 1 = July
monthIterate June n = monthIterate July ((n rem 12)-1)
monthIterate July 1 = August
monthIterate July n = monthIterate August ((n rem 12)-1)
monthIterate August 1 = September
monthIterate August n = monthIterate September ((n rem 12)-1)
monthIterate September 1 = October
monthIterate September n = monthIterate October ((n rem 12)-1)
monthIterate October 1 = November
monthIterate October n = monthIterate November ((n rem 12)-1)
monthIterate November 1 = December
monthIterate November n = monthIterate December ((n rem 12)-1)
monthIterate December 1 = January
monthIterate December n = monthIterate January ((n rem 12)-1)

//Start = monthIterate January 5 //June
//Start = monthIterate May 134 //July
//Start = monthIterate July 120000002 //September

//Write a function that takes a list of months and sorts them.
//Duplicates can be kept.

toNum :: Month -> Int
toNum January = 0
toNum February = 1
toNum March = 2
toNum April = 3
toNum May = 4
toNum June = 5
toNum July = 6
toNum August = 7
toNum September = 8
toNum October = 9
toNum November = 10
toNum December = 11

toMonth :: Int -> Month
toMonth 0 = January
toMonth 1 = February
toMonth 2 = March
toMonth 3 = April
toMonth 4 = May
toMonth 5 = June
toMonth 6 = July
toMonth 7 = August
toMonth 8 = September
toMonth 9 = October
toMonth 10 = November
toMonth 11 = December

monthSort :: [Month] -> [Month]
monthSort list = map toMonth(sort(map toNum list))
//Start = monthSort [February, October, January, June, December, May, April, October] //[January,February,April,May,June,October,October,December]
//Start = monthSort [] //[]

//Given a list of arrays, sort them by their highest number.
//The order of elements in the arrays must be preserved.
//In the case of arrays with equal highest numbers, their original order must be preserved.
sortArrays :: [{Int}] -> [{Int}]
sortArrays list = sortBy sortAux list
//Start = sortArrays [{4,2,5,6},{1,4,2},{5,2,1,0,3,2}] //[{1,4,2},{5,2,1,0},{4,2,5,6}]
//Start = sortArrays [{1,2,3},{2},{2,5,2},{3,1},{1,2},{0}] //[{0},{2},{1,2},{1,2,3},{3,1},{2,5,2}]
//Start = sortArrays [] //[]

sortAux :: {Int} {Int} -> Bool
sortAux a1 a2 = (maxArray a1) < (maxArray a2)
arrayToList :: {Int} -> [Int]
arrayToList array = [x\\x<-:array]
listToArray :: [Int] -> {Int}
listToArray list = {x\\x<-list}
maxArray :: {Int} -> Int
maxArray array = maxList(arrayToList array)

//Given a month, generate an array of months from that month til the end of the year.
//The months must be in order.
yearEnd :: Month -> {Month}
yearEnd m = {toMonth x\\x<-[(toNum m)..11]}
//Start = yearEnd January //{January,February,March,April,May,June,July,August,September,October,November,December}
//Start = yearEnd July //{July,August,September,October,November,December}
Start = yearEnd December //{December}