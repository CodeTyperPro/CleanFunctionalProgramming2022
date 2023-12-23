module endretakeMorning
import StdEnv
/* Functional Programming endterm retake, 2023. Dec 22.
-- This solution was submitted and prepared by
WRITE NAME AND NEPTUN HERE!! 
-- for the Functional Programming course.
-- I declare that this solution is my own work.. */
/* 1.----------
Given an email address (String) check if it is valid. 
Email address must comply with the following rules:
- It should start with a letter (not digit or symbol)
- It must contain the '@' symbol
- After the '@' symbol there should be a '.'
- There should be some string after the '.'
Help: use isAlpha to check if is a letter. 
*/
//isValid :: String -> Bool
  
//Start = isValid "email@gmail.com" // True
//Start = isValid "email.com" // False
//Start = isValid "2mail@mail.com" // False
//Start = isValid "mail@com" // False
//Start = isValid "mail@gmail." // False
//Start = isValid "Anna@g.com" // True
/* 2.----------
Given a list of string, remove every 3rd character 
from each string and return the result list.
Eg. ["abcdefgh", "cadcbdccd", "bacbacbac"]
result:  ["abdegh","cacbcc","bababa"]
*/
//removeChar :: [String] -> [String]
//Start = removeChar ["abcdefgh", "cadcbdccd", "bacbacbac"] // ["abdegh","cacbcc","bababa"]
//Start = removeChar ["fuencwtidonhal", "prxogeraqmmminyg"] // ["functional","programming"]
//Start = removeChar ["",""] // ["", ""]
//Start = removeChar [] // []
/* 3.---------- 
Given a card record with rank and suit. 
Rank is the value on the card (value from 2..10
or Ace, Jack, Queen, and King).
Suit can be Heart, Spade, Diamond, or Club.
Given a hand (list of cards) check if the hand is valid: 
there should not be the same cards multiple times and 
the card rank should have a valid value. 
Hint: toInt can be used on Strings with numeric values, 
toInt "King" is 0 (also for Jack, Queen, Ace).
Instance == for Suit and == for Card can be useful!
*/
:: Suit = Heart | Spade | Diamond | Club
:: Card = {rank :: String, suit :: Suit}
//handValid :: [Card] -> Bool
//Start = handValid [{rank = "5", suit = Diamond}, {rank = "Ace", suit = Diamond},{rank = "5", suit = Diamond}] // False
//Start = handValid [{rank = "King", suit = Spade}, {rank = "King", suit = Club}, {rank = "Ace", suit = Diamond},{rank = "5", suit = Diamond}] // True
//Start = handValid [{rank = "King", suit = Diamond}, {rank = "Ace", suit = Diamond},{rank = "13", suit = Diamond}] // False
/* 4.----------
Given a hand, calculate the sum value of the cards.
A card value is its numeric rank as integer,
and Jack=11, Queen=12, King=13 and Ace=14. 
*/
//sumHand :: [Card] -> Int
//Start = sumHand [{rank = "5", suit = Diamond}, {rank = "Ace", suit = Diamond},{rank = "5", suit = Diamond}] // 24
//Start = sumHand [{rank = "King", suit = Diamond}, {rank = "Ace", suit = Diamond},{rank = "5", suit = Diamond}] // 32
//Start = sumHand [{rank = "King", suit = Diamond}, {rank = "Ace", suit = Diamond},{rank = "10", suit = Diamond}] // 37
/* 5.----------
Given a hand sort the cards. First the ranking takes priority. 
2 < 3 < 4 ... < 10 < Jack < Queen < King < Ace
If the ranking is the same, then consider suit 
such that Club < Daimond < Spade < Heart 
*/
//sortHand :: [Card] -> [Card]
 
//Start = sortHand [{rank = "5", suit = Diamond}, {rank = "5", suit = Spade},{rank = "5", suit = Club}] // [(Card "5" Club),(Card "5" Diamond),(Card "5" Spade)]
//Start = sortHand [{rank = "King", suit = Diamond}, {rank = "Ace", suit = Diamond},{rank = "5", suit = Diamond}] // [(Card "5" Diamond),(Card "Ace" Diamond),(Card "King" Diamond)]
//Start = sortHand [{rank = "King", suit = Diamond}, {rank = "King", suit = Club},{rank = "King", suit = Heart}] // [(Card "King" Club),(Card "King" Diamond),(Card "King" Heart)]
 
/* 6.----------
Find if a tree is balanced. Take the first node and analyse 
the depth of its sub-trees. If the left one is greater, 
then return -1, if the right one is greater, return 1. 
If they are equal, return 0.
*/
:: Tree = Node Tree Tree | Leaf
//balance :: Tree -> Int
//Start = balance (Node (Node Leaf Leaf) Leaf) // -1
//Start = balance (Node Leaf (Node Leaf Leaf)) // 1
//Start = balance (Node (Node Leaf Leaf) (Node Leaf Leaf)) // 0
//Start = balance (Node (Node (Node Leaf (Node Leaf Leaf)) Leaf) (Node Leaf (Node Leaf Leaf))) // -1
 /* 7.----------  
Overload the (*) operator for lists of any type a elements. 
Return a list of the elements that are in the union of the 
list arguments. Sort the result and delete repetitions.
Eg. [1,2,3,4] * [8,1,3,5] = [1,2,3,4,5,8]
*/
// write instance 
//Start = [1,2,3,4] * [8,1,3,5] // [1,2,3,4,5,8]
//Start = [2,4,6,7,8] * [2,4,1,6,7,8,10] // [1,2,4,6,7,8,10]
//Start = [12,52,23,56,23,43,32] * [32,12,57,13] // [12,13,23,32,43,52,56,57]
//Start = ['k', 'z', 'a']*['c'..'g'] // ['a','c','d','e','f','g','k','z']
//Start = [21.8,12.5] * [] // [12.5,21.8]
//Start = [] * [1..5] // [1,2,3,4,5]
//Start :: [Real]
//Start = [] * []
/* 8.----------
Given Ocean and Sea data: Ocean has 2 fields name and array of seas. 
Sea has 3 fields: name, number of sharks and statistics that at the 
beginning is Unknown. Given an array of Oceans, update the statistics 
field for every sea in every ocean: update it to either 
High, Average or Low. Sea has high Statistics if its 
number of sharks is more than 15, Average if it has more than 5, 
otherwise - Low.
*/
:: Stats = High | Average | Low | Unknown 
:: Sea = {name :: String, numOfShark :: Int, statistics :: Stats}
:: Ocean = {name :: String, seas :: {Sea}}
 
sea1 :: Sea
sea1 = {name = "Bering Sea", numOfShark = 18, statistics = Unknown}
sea2 :: Sea
sea2 = {name = "Sea of Japan", numOfShark = 8, statistics = Unknown}
sea3 :: Sea
sea3 = {name = "Coral Sea", numOfShark = 2, statistics = Unknown}
sea4 :: Sea
sea4 = {name = "North Sea", numOfShark = 22, statistics = Unknown}
sea5 :: Sea
sea5 = {name = "Caribbean Sea", numOfShark = 13, statistics = Unknown}
sea6 :: Sea
sea6 = {name = "Barents Sea", numOfShark = 8, statistics = Unknown}
sea7 :: Sea
sea7 = {name = "Chukchi Sea", numOfShark = 3, statistics = Unknown}
ocean1 :: Ocean
ocean1 = {name="Pacific Ocean", seas={sea1, sea2, sea3}}
ocean2 :: Ocean
ocean2 = {name="Atlantic Ocean", seas={sea4, sea5}}
ocean3 :: Ocean
ocean3 = {name="Arctic Ocean", seas={sea6, sea7}}
//updateStatistics :: {Ocean} -> {Ocean}
//Start = updateStatistics {ocean1, ocean2, ocean3}
/*
{(Ocean "Pacific Ocean" {(Sea "Bering Sea" 18 High),(Sea "Sea of Japan" 8 Average),(Sea "Coral Sea" 2 Low)}),
(Ocean "Atlantic Ocean" {(Sea "North Sea" 22 High),(Sea "Caribbean Sea" 13 Average)}),
(Ocean "Arctic Ocean" {(Sea "Barents Sea" 8 Average),(Sea "Chukchi Sea" 3 Low)})}
*/
/* 9.----------
Find the total number of sharks in all the oceans.
*/
//sharks :: [Ocean] -> Int
//Start = sharks [ocean1, ocean2, ocean3] // 74
/* 10.----------
Word is type synonym of String. Given operator <==>, 
create an instance for Words which returns True if the 2 
words have the same the number upper and the same number 
of lower letters.
*/
// write the type 
 
class Wordops a 
where 
 (<==>) :: a a -> Bool 
// write instance 
 
//Start = ["sarah" <==> "sarah", "bOris" <==> "Boris", "functional" <==> "Functional", "abcde" <==> "abco", "haPPy" <==> "pLaYz"]  
// [True, True, False, False, True]
/* 11.----------
Given a binary search tree, change the BST to a binary Tree such that a key 
of a node becomes original key plus sum of all greater keys in the given BST.
Input:
          5
        / \
           / \
          /   \
        3    8
        / \   / \
           /   \ / \
          2 4   6 10
            Output:
          29
        / \
           /   \
          /    \
        36    18
        /  \   /  \
           / \ /   \
          38 33 24   10
   For example 2 got replaced by 38 in the output: 
   the sum of all greater keys present in the BST 
   and the original number is 38: 2 + 3 + 4 + 5 + 6 + 8 + 10 = 38
   3 got replaced by 36: 3 + 4 + 5 + 6 + 8 + 10 = 36
   5 got replaced by 29: 5 + 6 + 8 + 10 =  29
   ...
*/
:: BST a = NodeB a (BST a) (BST a) | LeafB
 
//transform :: (BST Int) -> (BST Int)
BsTree1 = (NodeB 5 (NodeB 3 (NodeB 2 LeafB LeafB) (NodeB 4 LeafB LeafB)) (NodeB 8 (NodeB 6 LeafB LeafB) (NodeB 10 LeafB LeafB) ))
BsTree2 = (NodeB 4 (NodeB 3 (NodeB 3 (NodeB 2 (NodeB 1 LeafB LeafB) LeafB) LeafB) (NodeB 4 LeafB LeafB)) (NodeB 5 (NodeB 5 LeafB LeafB) (NodeB 6 LeafB LeafB))) 
//Start = transform BsTree1
//(NodeB 29 (NodeB 36 (NodeB 38 LeafB LeafB) (NodeB 33 LeafB LeafB)) (NodeB 18 (NodeB 24 LeafB LeafB) (NodeB 10 LeafB LeafB)))
//Start = transform BsTree2
//(NodeB 24 (NodeB 30 (NodeB 30 (NodeB 32 (NodeB 33 LeafB LeafB) LeafB) LeafB) (NodeB 24 LeafB LeafB)) (NodeB 16 (NodeB 16 LeafB LeafB) (NodeB 6 LeafB LeafB)))
/* 12.----------
A Plane has ticket field, used to find its price. 
If Ticket is Bn Bn, then the price is 1000, 
if it is Eco Ecothen the price is 750. 
Plane has fields fuelPerKg and planeWeigth (integers), 
and it has array of Passengers. 
Passenger itself has pW and bW (integers passenger weight tand baggage weight). 
Given list of Planes sort the list according to profit:
profit = (ticket_price*passenger_amount)-fuelPerKg*(planeweight+summed_passenger_weight+summed_baggage_weight)
*/
:: Ticket = Bn | Eco
:: Passenger = {pW :: Int, bW :: Int}
:: Plane = {ticket :: Ticket, fuelPerKg :: Int, planeWeight :: Int, passengers :: {Passenger}}
ps1 :: Passenger
ps1 = {pW = 87, bW = 28}
ps2 :: Passenger
ps2 = {pW = 65, bW = 20}
ps3 :: Passenger
ps3 = {pW = 72, bW = 32}
ps4 :: Passenger
ps4 = {pW = 95, bW = 25}
ps5 :: Passenger
ps5 = {pW = 80, bW = 30}
ps6 :: Passenger
ps6 = {pW = 60, bW = 18}
ps7 :: Passenger
ps7 = {pW = 75, bW = 22}
ps8 :: Passenger
ps8 = {pW = 88, bW = 27}
ps9 :: Passenger
ps9 = {pW = 78, bW = 35}
ps10 :: Passenger
ps10 = {pW = 70, bW = 23}
pl1 :: Plane
pl1 = {ticket = Bn, fuelPerKg = 11, planeWeight = 9500, passengers = {ps1, ps2, ps3}}
pl2 :: Plane
pl2 = {ticket = Eco, fuelPerKg = 9, planeWeight = 9000, passengers = {ps4, ps5, ps6}}
pl3 :: Plane
pl3 = {ticket = Bn, fuelPerKg = 12, planeWeight = 9800, passengers = {ps7, ps8, ps9}}
pl4 :: Plane
pl4 = {ticket = Eco, fuelPerKg = 10, planeWeight = 9200, passengers = {ps10, ps1, ps2}}
pl5 :: Plane
pl5 = {ticket = Bn, fuelPerKg = 11, planeWeight = 9600, passengers = {ps3, ps4, ps5}}
pl6 :: Plane
pl6 = {ticket = Eco, fuelPerKg = 9, planeWeight = 9100, passengers = {ps6, ps7, ps8}}
pl7 :: Plane
pl7 = {ticket = Bn, fuelPerKg = 12, planeWeight = 9900, passengers = {ps9, ps10, ps1}}
pl8 :: Plane
pl8 = {ticket = Eco, fuelPerKg = 10, planeWeight = 9300, passengers = {ps2, ps3, ps4}}
pl9 :: Plane
pl9 = {ticket = Bn, fuelPerKg = 11, planeWeight = 9700, passengers = {ps5, ps6, ps7}}
pl10 :: Plane
pl10 = {ticket = Eco, fuelPerKg = 9, planeWeight = 9200, passengers = {ps8, ps9, ps10}}
//sortPlanes :: [Plane] -> [Plane]
//Start = sortPlanes [pl1,pl2,pl3,pl4,pl5,pl6,pl7,pl8,pl9,pl10]
/*
[(Plane Bn 12 9900 {(Passenger 78 35),(Passenger 70 23),(Passenger 87 28)}),
(Plane Bn 12 9800 {(Passenger 75 22),(Passenger 88 27),(Passenger 78 35)}),
(Plane Bn 11 9700 {(Passenger 80 30),(Passenger 60 18),(Passenger 75 22)}),
(Plane Bn 11 9600 {(Passenger 72 32),(Passenger 95 25),(Passenger 80 30)}),
(Plane Bn 11 9500 {(Passenger 87 28),(Passenger 65 20),(Passenger 72 32)}),
(Plane Eco 10 9300 {(Passenger 65 20),(Passenger 72 32),(Passenger 95 25)}),
(Plane Eco 10 9200 {(Passenger 70 23),(Passenger 87 28),(Passenger 65 20)}),
(Plane Eco 9 9200 {(Passenger 88 27),(Passenger 78 35),(Passenger 70 23)}),
(Plane Eco 9 9100 {(Passenger 60 18),(Passenger 75 22),(Passenger 88 27)}),
(Plane Eco 9 9000 {(Passenger 95 25),(Passenger 80 30),(Passenger 60 18)})]
*/