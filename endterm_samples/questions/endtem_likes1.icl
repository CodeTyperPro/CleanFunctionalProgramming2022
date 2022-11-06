module endtem_likes1
import StdEnv

::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={name::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}

ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BMI::University
BMI={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}

Peter::Teacher
Peter={name="Peter",subject="Functional"}
Viktor::Teacher
Viktor={name="Viktor",subject="Math"}
Mary::Teacher
Mary={name="Mary",subject="OOP"}
John::Teacher
John={name="John",subject="Functional"}

Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}


/*1 Given a University, return an array of all the 
students names which have gpa greater than 4, and a favorite teacher who teaches Functional*/

gpaAndFavoriteTeacher::University->{String}
gpaAndFavoriteTeacher uni={y.studentName \\y<-uni.students | (toReal(sum [a\\a<-:y.grades]))/(toReal(length [a\\a<-:y.grades]))>4.0 && y.favoriteTeacher.subject=="Functional"}

//Start=gpaAndFavoriteTeacher BMI//{"Josh","Sofi"}
//Start=gpaAndFavoriteTeacher ELTE//{"Josh"}
//Start=gpaAndFavoriteTeacher EmptyUni//{}


/*2 Given a University, return an array of all the 
students or teachers names which are shorter than 6*/

shorterThan6::University->{String}
shorterThan6 uni={b\\b<-[y.studentName\\y<-(uni.students)|length[a\\a<-:y.studentName]<6]++[y.name\\y<-(uni.teachers)|length[a\\a<-:y.name]<6]}

//Start=shorterThan6 BMI//{"Ana","Josh","Sofi","John","Peter"}
//Start=shorterThan6 ELTE//{"Marko","Josh","Dame","Mary","Peter","John"}
//Start=shorterThan6 EmptyUni//{}


/*3 Write a function which will take an array of Universities and return the University with the highest overall gpa (the average of the average of each student)*/

highestGpa::{University}->String
highestGpa unis
|length x == 0="No universities given"
= (last (sort x)).uniName
where x = [y\\y<-:unis]

instance < University 
where 
 (<) a b = calcGpa a < calcGpa b

instance == University 
where 
 (==) a b = calcGpa a == calcGpa b

calcGpa::University->Real
calcGpa a
|length a.students ==0=0.0
=(sum [studentGpa y\\y<-a.students])/toReal(length a.students)

studentGpa::Student->Real 
studentGpa stud = toReal(sum x)/(toReal (length x))
where x = [y\\y<-:(stud.grades)]

//Start=studentGpa Nikola


//Start=highestGpa {ELTE,BMI,EmptyUni}//"BMI"
//Start=highestGpa {ELTE,BMI} //"BMI"
//Start=highestGpa {EmptyUni,EmptyUni}//"Empty"
//Start=highestGpa {ELTE} //"ELTE"
//Start=highestGpa {}//"No universities given"

/*4	Write an instance of type Student, such that two students are equal if their gpa differs in less than 0.3 and they have the same favorite teacher*/

instance == Student
where 
	(==) a b = (abs (studentGpa a -studentGpa b))<0.3 && a.favoriteTeacher.name==b.favoriteTeacher.name

//Start= Nikola == Nik//True
//Start=Nikola == Nikola //True
//Start= Nik== Nik2//False
//Start= Nikola == Nik2//False

/*5	Create a toString instance for Student such that for given student ex. Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},
favoriteTeacher=Peter} it gives "Nikola 3.6 Peter" where 3.6 is the student's gpa and Peter is the student's favorite teacher's name*/

instance toString Student
where toString a=a.studentName+++"  "+++toString (studentGpa a)+++"  "+++a.favoriteTeacher.name

//Start=toString Nikola//"Nikola  3.6  Peter"
//Start=toString Marko//"Marko  4.25  Mary"
//Start=toString Nik//"Nik  3.8  Peter"
//Start=toString Dame//"Dame  3.5  Peter"

/*6 Create an * instance of lists such that list1 * list2 will give a list of pairwise product of the two lists and if the length 
of one list is greater than the other one just add the remaining elements to the end of the new list*/

fun::[a] [a] -> [a] | * a
fun [] [] = []
fun a b
|length a>=length b=[x*y\\ x<-a & y<-b] ++( drop (length b) a)
=fun b a

instance * [a] | * a
where
	(*) a b = fun a b
	
//Start=[1,2]*[3,4,5,6,0]//[3,8,5,6,0]
//Start= [1,2,3,1,3,12,312] *[2,3]//[2,6,3,1,3,12,312]
Start :: [Int]
Start= [] * []//[]