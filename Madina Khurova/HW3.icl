module HW3
import StdEnv

//Please Rename the file as YourNameHW3.icl

/*

    You're safe online if all your passwords are strong.
    A password is strong if it is at least 8 characters long and contains at 
    least one uppercase letter, one lowercase letter, one digit and 
    one special character. 
    The special characters are: !@#$%^&*()_+-=[]{}|;':",./<>?~`

    Write a function strongPassword that takes a list of passwords and checks if 
    all your passwords are strong.

    Eg : Input : ["Hello@World9", "HelloWorld1!", "HelloWorld1!"]
        Output : "All passwords are strong"

        Input : ["JohnDow","Hellom","yotoo@123"]
        Output : "All passwords are not strong"

        Hint : use the function fromString x to convert string x to a list of char.

        **** Please dont put your own passwords as a test case ****
*/

hasDigit :: [Char] -> Bool
hasDigit [] = False
hasDigit [x:xs] = (isMember x ['0','1' .. '9']) || hasDigit xs


hasUpper :: [Char] -> Bool
hasUpper [] = False
hasUpper [x:xs] = (isMember x ['A', 'B' .. 'Z']) || hasUpper xs

hasLower :: [Char] -> Bool
hasLower [] = False
hasLower [x:xs] = (isMember x ['a', 'b' .. 'z']) || hasLower xs

hasSpecial :: [Char] -> Bool
hasSpecial [] = False
hasSpecial [x:xs] = (isMember x (fromString ("!@#$%^&*()_+-=[]{}|;':,./<>?~`]"))) || hasSpecial xs
// " ????


checkPass :: String -> Bool
checkPass x = ((length s) >= 8) &&(hasDigit s) && (hasUpper s) && (hasLower s) && (hasSpecial s)
where s = (fromString (x))

strongPassword :: [String] -> String
strongPassword x
| (foldr (&&) True (map checkPass x)) == True = "All passwords are strong"
= "All passwords are not strong"

//Start = strongPassword ["Hello@World9", "HelloWorld1!", "Helloworld@123"] // "All passwords are strong"
//Start = strongPassword ["JohnDow","Hellom","yotoo@123"] // "All passwords are not strong"


/*
    Write a function that takes a list of Real numbers and returns a list of lists 
    where each sublist contains two numbers where first number is the element from the list 
    and the second number is the percentage of frequency of that element in the list.

    eg : Input : [1,2,1,3] 
        Output : [[1,50],[2,25],[3,25]]
        explanation : 1 -> occurs 2 times in the list , total elements in the list = 4 so its percentage is 50
                    2 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25
                    3 -> occurs 1 time in the list , total elements in the list = 4 so its percentage is 25

        percentage as integer is fine
*/

frequencyAux :: [Int] Int -> [[Int]]
frequencyAux [] _ = []
frequencyAux [x:xs] s = [[x, ((length (filter ((==) x) [x:xs]))*100)/s]] ++ (frequencyAux y s)
where y = filter ((<>) x) [x:xs]

frequency :: [Int] -> [[Int]]
frequency [] = []
frequency x = frequencyAux x y
where y = length x

Start = frequency [1,2,1,3]  // [[1,50],[2,25],[3,25]]
//Start = frequency [1,1,1,3,2,2] // [[1,50],[2,33],[3,16]]
//Start = frequency [1,2,5,5,1,1,0,0] // [[1,37],[2,12],[5,25],[0,25]]