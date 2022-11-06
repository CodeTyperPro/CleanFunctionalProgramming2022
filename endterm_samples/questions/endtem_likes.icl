module endtem_likes
import StdEnv

:: Gender = Male | Female | Nghia | AttackHelicopter | OOBLECK
:: Person = {givenName :: String, lastName :: String, gender :: Gender}
:: FamilyTree = Name Person FamilyTree FamilyTree | End | Polygamy [[[[[[[[[[[FamilyTree]]]]]]]]]]]

instance == Gender
where
    (==) Male Male = True
    (==) Female Female = True
    (==) Nghia Nghia = True
    (==) AttackHelicopter AttackHelicopter = True
    (==) OOBLECK OOBLECK = True
    (==) _ _ = False

instance == Person
where
    (==) p1 p2 = and[p1.givenName == p2.givenName, p1.lastName == p2.lastName, p1.gender == p2.gender]

instance == FamilyTree
where
    (==) End End = True
    (==) (Name x1 l1 r1) (Name x2 l2 r2) = and[ x1==x2, l1==l2, r1==r2]
    (==) _ _ = False

Pedro :: Person
Pedro = {givenName = "Pedro Henrique", lastName = "Villar deFigueiredo", gender = Male}
Mauro :: Person
Mauro = {givenName = "Mauro", lastName = "daRocha Carvalho", gender = Male}
Joao :: Person
Joao = {givenName = "Joao", lastName = "Pereira Cavalcanti", gender = Male}
Carlos :: Person
Carlos = {givenName = "Carlos", lastName = "Teixeira deAndrade", gender = Male}
Luiz :: Person
Luiz = {givenName = "Luiz", lastName = "Barroso Mourao", gender = Male}
Leoberto :: Person
Leoberto = {givenName = "Leoberto", lastName = "Praxedes Santos", gender = Male}
Luan :: Person
Luan = {givenName = "Luan", lastName = "deRosas Lima", gender = Male}
Matheus :: Person
Matheus = {givenName = "Matheus", lastName = "Andrade Duarte", gender = Male}
AnaMaria :: Person
AnaMaria = {givenName = "Ana Maria", lastName = "Silva Figueira", gender = Female}
Lucia :: Person
Lucia = {givenName = "Lucia", lastName = "Elena Paiva", gender = Female}
Elena :: Person
Elena = {givenName = "Elena Maria", lastName = "Lacerda Leite", gender = Female}
Vitoria :: Person
Vitoria = {givenName = "Vitoria", lastName = "Correia Negrao", gender = Female}
Miriam :: Person
Miriam = {givenName = "Miriam", lastName = "Marinho Silva", gender = Female}
Veronica :: Person
Veronica = {givenName = "Veronica", lastName = "Soares deCarvalho", gender = Female}
Olivia :: Person
Olivia = {givenName = "Olivia", lastName = "Alves daSilva", gender = Female}
Maria :: Person
Maria = {givenName = "Maria Luiza", lastName = "Gama Pordeus", gender = Female}
Bruna :: Person
Bruna = {givenName = "Bruna", lastName = "Melo Guedes", gender = Female}

PedroFamily :: FamilyTree
PedroFamily = Name Pedro MauroFamily LuciaFamily

MauroFamily :: FamilyTree
MauroFamily = Name Mauro JoaoFamily ElenaFamily
LuciaFamily :: FamilyTree
LuciaFamily = Name Lucia CarlosFamily VitoriaFamily

JoaoFamily :: FamilyTree
JoaoFamily = Name Joao LuizFamily MiriamFamily
ElenaFamily :: FamilyTree
ElenaFamily = Name Elena LeobertoFamily VeronicaFamily
CarlosFamily :: FamilyTree
CarlosFamily = Name Carlos LuanFamily OliviaFamily
VitoriaFamily :: FamilyTree
VitoriaFamily = Name Vitoria MatheusFamily MariaFamily

LuizFamily :: FamilyTree
LuizFamily = Name Luiz End End
MiriamFamily :: FamilyTree
MiriamFamily = Name Miriam End End
LeobertoFamily :: FamilyTree
LeobertoFamily = Name Leoberto End End
VeronicaFamily :: FamilyTree
VeronicaFamily = Name Veronica End End
LuanFamily :: FamilyTree
LuanFamily = Name Luan End End
OliviaFamily :: FamilyTree
OliviaFamily = Name Olivia End End
MatheusFamily :: FamilyTree
MatheusFamily = Name Matheus End End
MariaFamily :: FamilyTree
MariaFamily = Name Maria End End

fixedTree :: FamilyTree
fixedTree = (Name {givenName = "Pedro Henrique", lastName = "Lima Mourao", gender = Male} (Name {givenName = "Mauro", lastName = "Santos Mourao", gender = Male} (Name {givenName = "Joao", lastName = "Silva Mourao", gender = Male} (Name {givenName = "Luiz", lastName = "Barroso Mourao", gender = Male} End End )(Name {givenName = "Miriam", lastName = "Marinho Silva", gender = Female} End End ))(Name {givenName = "Elena Maria", lastName = "deCarvalho Santos", gender = Female} (Name {givenName = "Leoberto", lastName = "Praxedes Santos", gender = Male} End End )(Name {givenName = "Veronica", lastName = "Soares deCarvalho", gender = Female} End End )))(Name {givenName = "Lucia", lastName = "Duarte Lima", gender = Female} (Name {givenName = "Carlos", lastName = "daSilva Lima", gender = Male} (Name {givenName = "Luan", lastName = "deRosas Lima", gender = Male} End End )(Name {givenName = "Olivia", lastName = "Alves daSilva", gender = Female} End End ))(Name {givenName = "Vitoria", lastName = "Pordeus Duarte", gender = Female} (Name {givenName = "Matheus", lastName = "Andrade Duarte", gender = Male} End End )(Name {givenName = "Maria Luiza", lastName = "Gama Pordeus", gender = Female} End End ))))

/*
Would you kindly write a function that takes
a FamilyTree and returns the FamilyTree with 
the last names of everyone fixed.
In a FamilyTree, the root is the child,
and the left and right nodes are the parents.
Everyone here is Brazilian.
Each person's last name has two parts.
The first part comes from the mother's last name's second part,
the second part comes from the father last name's second part.
For example, if the parents are:
Mother -> Olivia Alves daSilva
Father -> Mauro daRocha Carvalho
Child -> Pedro daSilva Carvalho
If the Child has no parents, then do not fix their last name.
For simplicity sake, assume every child will have 2 Parents or none.
*/

getPerson :: FamilyTree -> Person
getPerson (Name x _ _) = x

parseLastName :: Person -> (String,String)
parseLastName p = result
where
    l = [x\\x<-:p.lastName]
    (a,b) = span (\x = x<>' ') l
    nameA = {x\\x<-a}
    nameB = {x\\x<-(tl b)}
    result = (nameA, nameB)

//Start = parseLastName {givenName = "Pedro Henrique", lastName = "Villar deFigueiredo", gender = Male}

concatLastNames :: String String -> String
concatLastNames a b = result
where
    la = [x\\x<-:a]
    lb = [x\\x<-:b]
    together = la ++ [' '] ++ lb
    result = {x\\x<-together}
//Start = concatLastNames "Hello" "World"

//Start = size ""

getDepth :: FamilyTree -> Int 
getDepth End = 0
getDepth (Name x l r) = 1 + max (getDepth l) (getDepth r)

//Start = getDepth PedroFamily

fixLastNames :: FamilyTree -> FamilyTree
fixLastNames t = (iterate fixLastNamesAux t) !! (getDepth t)

fixLastNamesAux :: FamilyTree -> FamilyTree
fixLastNamesAux End = End
fixLastNamesAux (Name p l r)
| l == End || r == End = Name p l r
= Name fixedPerson (fixLastNames l) (fixLastNames r)
where
    (_,a) = parseLastName (getPerson l)
    (_,b) = parseLastName (getPerson r)
    newLastName
    | (getPerson r).gender == Male = concatLastNames a b
    = concatLastNames b a
    fixedPerson = {p & lastName = newLastName}

instance toString Gender
where
    toString Male = "Male"
    toString Female = "Female"

instance toString FamilyTree
where
    toString End = "End "
    toString (Name x l r) = "(Name "+++"{givenName = \""+++x.givenName+++"\", lastName = \""+++x.lastName+++"\", gender = "+++toString x.gender+++"} "+++toString l+++ toString r+++")"
//Start = fixLastNames PedroFamily == fixedTree //True


:: Router = { nodeName :: String, activeStatus :: Bool}
:: Network = Node Router Network Network | Termination

r1 :: Router
r1 = {nodeName = "PL1", activeStatus = True}
r2 :: Router
r2 = {nodeName = "PL2", activeStatus = True}
r3 :: Router
r3 = {nodeName = "PL3", activeStatus = False}
r4 :: Router
r4 = {nodeName = "PL4", activeStatus = True}
r5 :: Router
r5 = {nodeName = "PL5", activeStatus = False}
r6 :: Router
r6 = {nodeName = "PL6", activeStatus = True}
r7 :: Router
r7 = {nodeName = "PL7", activeStatus = True}
r8 :: Router
r8 = {nodeName = "PL8", activeStatus = False}
r9 :: Router
r9 = {nodeName = "PL9", activeStatus = False}
r10 :: Router
r10 = {nodeName = "PL10",  activeStatus = False}
r11 :: Router
r11 = {nodeName = "PL11",  activeStatus = True}
r12 :: Router
r12 = {nodeName = "PL12",  activeStatus = True}
r13 :: Router
r13 = {nodeName = "PL13",  activeStatus = False}
r14 :: Router
r14 = {nodeName = "PL14",  activeStatus = True}
r15 :: Router
r15 = {nodeName = "PL15",  activeStatus = True}
r16 :: Router
r16 = {nodeName = "PL16",  activeStatus = True}
r17 :: Router
r17 = {nodeName = "PL17",  activeStatus = True}
r18 :: Router
r18 = {nodeName = "PL18",  activeStatus = False}
r19 :: Router
r19 = {nodeName = "PL19",  activeStatus = True}
r20 :: Router
r20 = {nodeName = "PL20",  activeStatus = False}
r21 :: Router
r21 = {nodeName = "PL21",  activeStatus = False}
r22 :: Router
r22 = {nodeName = "PL22",  activeStatus = True}

noNetwork :: Network
noNetwork = Termination
oneNetwork :: Network
oneNetwork = Node r1 Termination Termination
smolNetwork :: Network
smolNetwork = Node r1 (Node r2 (Node r4 Termination Termination)(Node r5 Termination Termination))(Node r3 (Node r6 Termination Termination) (Node r7 Termination Termination))
bigNetwork :: Network
bigNetwork = Node r11 (Node r4 (Node r3 (Node r1 Termination Termination) Termination) (Node r6 (Node r5 Termination Termination) Termination)) (Node r16 (Node r14 Termination (Node r15 Termination Termination)) (Node r19 (Node r18 Termination Termination) (Node r21 (Node r20 Termination Termination) (Node r22 Termination Termination))))
/*
Would you kindly write a function, that takes a Network
and a name of a Router, and returns a Bool indicating
if that Router is sucessfully connected.
A Router is successfully connected if ALL NODES
from the root to the router node have activeStatus of True.
If the Router is not found, return False.
*/
pathCheck :: Network String -> Bool
pathCheck Termination _ = False
pathCheck (Node x l r) n
| x.nodeName == n && x.activeStatus = True
| not x.activeStatus = False
= or[pathCheck l n, pathCheck r n]

Start = pathCheck smolNetwork "PL4" //True
//Start = pathCheck smolNetwork "PL69" //False
//Start = pathCheck bigNetwork "PL15" //True
//Start = pathCheck bigNetwork "PL22" //False
//Start = pathCheck oneNetwork "PL1" //True
//Start = pathCheck noNetwork "HelloGoodBye" //False


