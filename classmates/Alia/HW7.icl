module HW7
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

:: Item = {
			key::String,
			value::Int
			}
			
// you can test by changing some values inside t1 tree, instead of creating multiple trees for test cases.
			
t1 :: Tree Item
t1 = Node {key="a",value=5} (Node {key="b",value=2} (Node {key="x",value=10} (Node {key="h", value=3} Leaf Leaf) Leaf) (Node {key="y", value=7} Leaf Leaf)) (Node {key="d", value=8} (Node {key="e",value=15} (Node {key="g", value=12} Leaf Leaf) Leaf) (Node {key="f",value=9} Leaf Leaf))
			
/*
 1. Given a tree of Item. Find the key of the item that has the maximum value.
	All the key inside tree are unique.
	
	You can define the new instance for comparing Item or you can just simply write a function comparing two items.
*/

extract :: (Tree Item) -> Item
extract Leaf = {key="",value=0}
extract (Node x l r) = x

//toString

instance < Item
where
	(<) x y = x.value < y.value

instance < (Tree Item)
where
	(<) x y = (extract x) < (extract y)

getKey :: Item -> String
getKey x = x.key

r1 :: Tree Item
r1 = Node {key="a",value=5} (Node {key="b",value=2} Leaf Leaf) (Node {key="x",value=10} Leaf Leaf)

getMax :: Item Item -> Item
getMax x y
| x < y = y
= x

searchMaxKeyAux :: (Tree Item) -> Item
searchMaxKeyAux Leaf = {key="",value=1}
searchMaxKeyAux (Node x Leaf Leaf) = x
searchMaxKeyAux (Node x le ri) = getMax x (getMax (searchMaxKeyAux le) (searchMaxKeyAux ri))

searchMaxKey :: (Tree Item) -> String
searchMaxKey Leaf = ""
searchMaxKey x = getKey (searchMaxKeyAux x)

Start = searchMaxKey t1 // "e"