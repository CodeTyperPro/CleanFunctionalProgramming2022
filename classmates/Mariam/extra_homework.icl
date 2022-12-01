module extra_homework
import StdEnv

::Movie = {
			title::String,
			casts::[Cast],
			year::Int,
			rating::Real,
			country::String
			}
::Gender = Male | Female
::Cast = {
			name::String,
			gender::Gender
			}

cast1 = { name="Jackie Chan", gender= Male}
cast2 = { name="Jet Li", gender= Male }
cast3 = { name="Millie Bobby Brown", gender=Female}
cast4 = { name="Chris Hemsworth", gender=Male }
cast5 = { name="Zendaya", gender=Female }
cast6 = { name="Emma Stone", gender=Female }
cast7 = { name="Emma Watson", gender = Female }
cast8 = { name="Sandra Bullock", gender = Female}
cast9 = { name="Chris Evans", gender= Male}
cast10 = { name="Tom Holland", gender=Male}
cast11 = { name="Tobey Maguire", gender=Male}
cast12 = { name="Bae Suzy", gender=Female}
cast13 = { name="Park Seo Joon", gender=Male}

movie1 = { title="MOVIE I.", casts=[cast1, cast6, cast10], year=2019, rating=8.5, country="USA"}
movie2 = { title="MOVIE II.", casts=[cast4, cast8, cast7], year=2020, rating=8.0, country="Spain"}
movie3 = { title="MOVIE III.", casts=[cast13, cast12, cast9], year=2019, rating=9.0, country="Korea"}
movie4 = { title="MOVIE IV.", casts=[cast3, cast11, cast8], year=2021, rating=6.5, country="India"}
movie5 = { title="MOVIE V.", casts=[cast2, cast4, cast10], year=2022, rating=7.4, country="Hungary"}
movie6 = { title="MOVIE VI.", casts=[cast3, cast5, cast8], year=2022, rating=7.4, country="Hungary"}


movieList = [movie1,movie2,movie3,movie4,movie5, movie6]

/*
    How many casts participated in more than 1 movie (if cast4 is participated in more than one movie, then it should be counted as 1)
 	The number of casts who participated in more than one movie.
 	The solution should not rely on the fact that you know the list of casts.

*/

instance == Gender
where
	(==) Male Male = True
	(==) Female Female = True
	(==) _ _ = False

instance == Cast
where
	(==) x y = x.name == y.name && x.gender == y.gender

isGood :: Cast [Cast] -> Bool
isGood _ [] = False
isGood x [y:xs]
| x == y = True
= isGood x xs

removeCounted:: Cast [Cast] -> [Cast]
removeCounted _ [] = []
removeCounted x [y:xs]
| x == y = removeCounted x xs
= [y] ++ removeCounted x xs

computeCountOfCast:: [Cast] -> Int
computeCountOfCast [] = 0
computeCountOfCast [x:xs]
| (isGood x xs) = 1 + (computeCountOfCast removed)
= computeCountOfCast xs
where
	removed = (removeCounted x xs)

countOfCast:: [Movie] -> Int
countOfCast [] = 0
countOfCast list_of_movies = computeCountOfCast list_of_casts
where
	list_of_casts = flatten [[j \\ j<- i.casts] \\ i<- list_of_movies]
 
Start = countOfCast movieList // 4