module testing
import StdEnv

::Person={name::String, mass::Real, height::Real, bmi::Real}

Rose::Person
Rose={name="Rose", mass=147.71, height=1.72, bmi=0.0}
Jack::Person
Jack={name="Jack", mass=158.73, height=1.93, bmi=0.0}
Emilia::Person
Emilia={name="Emilia", mass=121.25, height=1.60, bmi=0.0}
Leo::Person
Leo={name="Leo", mass=85.98, height=1.75, bmi=0.0}
Grace::Person
Grace={name="Grace", mass=112.43, height=1.65, bmi=0.0}
Harry::Person
Harry={name="Harry", mass=169.76, height=1.80, bmi=0.0}

/*2.b(65 pts)-Given an array of Persons, write a function that calculates the BMI of each Person
BMI: body mass index = m / h^2
m = mass (in kilograms)
h = height (in meters)

note: the mass given in the records are in pounds, you need to convert before using the formula
hint: 1 pound = 0.453592kg
*/

adjustHeight :: Real -> Real
adjustHeight height = height*100.0

poundToKg :: Real -> Real
poundToKg mass = (mass*0.453592)

bodyMassIndex :: Real Real -> Real
bodyMassIndex mass height = (x/(y*y))*10000.0
where 
    x = poundToKg mass
    y = adjustHeight height

calcBMI :: {Person} -> {Person}
calcBMI array = { {name = x.name, mass = (poundToKg x.mass), height = (adjustHeight x.height), bmi = (bodyMassIndex x.mass x.height)} \\ x <-: array}

Start = calcBMI {Rose,Jack,Emilia} // {(Person "Rose" 67 172 22.6473769605192),(Person "Jack" 72 193 19.3293779698784),(Person "Emilia" 55 160 21.484375)}
//Start = calcBMI {Leo,Grace,Harry} // {(Person "Leo" 39 175 12.734693877551),(Person "Grace" 51 165 18.732782369146),(Person "Harry" 77 180 23.7654320987654)}
//Start = calcBMI {} // {}