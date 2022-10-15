module ex3_todo2_HEIOPO
import StdEnv

/*
	NEPTUNE: HEIOPO | ALFREDO MARTINS
*/

// 19.* (bonus) Compute the Euler number approximation in n steps: e = 1/0! + 1/1! + 1/2! + 1/3! + ... 
// do not compute factorial 

fllAux :: Real Int Int -> Real
fllAux x y n
| y == n = u
= u + (fllAux u (y+1) n)
where u = x*(toReal (toReal (1)/(toReal (y))))

fll :: Int -> Real
fll x = fllAux 1.0 1 x

Start = fll 1000