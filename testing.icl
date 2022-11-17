module testing
import StdEnv

/* 1.
 Create an instances +, -, <,<>, == for RGBColor
 + should add respective parameters
 - should subtract respective parameters
 == is true if all three parameters are equal 
 <> is false if all three parameters are equal, true otherwise.
 < Compare them lexicographically (if reds are equal compare greens and so on)
*/

Eq

:: RGBColor = {r :: Int, g :: Int, b :: Int}

instance + RGBColor
where
    (+) :: !RGBColor !RGBColor -> RGBColor
    (+) x y = {r = x.r + y.r, g = x.g + y.g, b = x.b + y.b}

instance - RGBColor
where
    (-) :: !RGBColor !RGBColor -> RGBColor
    (-) x y = {r = x.r - y.r, g = x.g - y.g, b = x.b - y.b}

instance < RGBColor
where
    (<) :: !RGBColor !RGBColor -> Bool
    (<) x y = (x.r + x.g + x.b) < (y.r + y.g + y.b)

instance == RGBColor
where
    (==) :: !RGBColor !RGBColor -> Bool
    (==) x y = (x.r == y.r) && (x.g == y.g) && (x.b == y.b)

class Diff a | == a
where
    (<>) infix 4 :: a a -> Bool | Diff a
    (<>) x  y = not (x == y)

toString

//Start = {r = 0, g = 0, b = 0} == {r = 0, g = 0, b = 0} // True
//Start = {r = 0, g = 0, b = 0} <> {r = 0, g = 0, b = 0} // False
//Start = {r = 30, g = 150, b = 231} == {r = 10, g = 30, b = 231} // False
//Start = {r = 30, g = 150, b = 231} - {r = 1, g = 1, b = 1} // {r = 29, g = 149, b = 230}
//Start = {r = 30, g = 150, b = 231} + {r = 1, g = 1, b = 1} // {r = 31, g = 152, b = 232}
//Start = {r = 30, g = 150, b = 231} < {r = 10, g = 30, b = 231} // False
//Start = {r = 30, g = 150, b = 231} < {r = 30, g = 150, b = 231} // False
//Start = {r = 30, g = 150, b = 231} < {r = 30, g = 151, b = 231} // True