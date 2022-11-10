module PT7
import StdEnv

/* Given the Truck type, write new instances so that you can sort the truck by its
average reputation score. 

bmw1 < bmw2 because bmw1.reputation(avg) = 2.833333
					   bmw2.reputation(avg) = 2.3333333
*/

:: TruckType = TOYOTA | BMW | KIA
:: Truck = {name::String, type :: TruckType, reputation:: [Int]}

toyota1::Truck
toyota1 = {name="toyota1",type=TOYOTA, reputation =[5,5,3,4,2,4,5,5]} 
toyota2::Truck
toyota2 = {name="toyota2",type=TOYOTA, reputation=[5,5,5,5,3,4,5,4,5]}

bmw1::Truck
bmw1 = {name="bmw1",type=BMW, reputation =[3,2,3,4,2,4,2,1,4,3,2,4]}
bmw2::Truck
bmw2 = {name="bmw2",type=BMW, reputation=[1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]}

kia1::Truck
kia1 = {name="kia1",type=KIA, reputation=[1,2,2,3,1,3,4,2,3,4,2,4,2,1]} 
kia2::Truck
kia2 = {name="kia2",type=KIA, reputation=[3,4,1,3,4,2,3,5,5]}



instance == Truck
where
	(==) a b = (avg a.reputation) == (avg a.reputation)

instance < Truck
where
	(<) a b = (avg a.reputation) == (avg a.reputation)

helper :: [Truck] -> [Real]
helper list = l
where
	l = map avg [map toReal x.reputation \\ x <- list]

//Start = helper [toyota1, toyota2, bmw1, bmw2, kia1,kia2]

sortTruck :: [Truck] -> [Truck]
sortTruck list = sort list
where
	l = helper list

//Start = sortTruck [toyota1, toyota2, bmw1, bmw2, kia1,kia2] // [(Truck "bmw2" BMW [1,2,1,3,1,5,3,3,4,1,3,1,5,1,1]),(Truck "kia1" KIA [1,2,2,3,1,3,4,2,3,4,2,4,2,1]),(Truck "bmw1" BMW [3,2,3,4,2,4,2,1,4,3,2,4]),(Truck "kia2" KIA [3,4,1,3,4,2,3,5,5]),(Truck "toyota1" TOYOTA [5,5,3,4,2,4,5,5]),(Truck "toyota2" TOYOTA [5,5,5,5,3,4,5,4,5])]
