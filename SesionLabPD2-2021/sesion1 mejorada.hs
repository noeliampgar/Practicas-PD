--Noelia de las Mercedes Pérez García-Consuegra

--1)Define funciones recursivas

--1.a)
cuadrados:: (Num a, Eq a) => a -> [a]

cuadrados 0 = [0]
cuadrados n = cuadrados (n-1) ++ [n^2]


--1.b)
cuadradosEmparejados:: (Num a, Eq a) => a -> [(a,a)]

cuadradosEmparejados 0 = [(0,0)]
cuadradosEmparejados n = [(n,n^2)] ++ cuadradosEmparejados (n-1)


--1.c)
sumaCos :: (Eq p, Floating p) => p -> p

sumaCos 1 = abs(cos(1))
sumaCos n = sumaCos (n-1) + n * abs(cos(n))


--1.d)
multiplosMenOEq :: Integral a => a -> a

multiplosMenOEq 1 = 0
multiplosMenOEq n = if (mod n 3 == 0 || mod n 5 == 0)
                 then n + multiplosMenOEq (n-1)
                 else multiplosMenOEq (n-1)

multiplosMen :: Integral a => a -> a

multiplosMen n = multiplosMenOEq (n-1)



--2)Define funciones para calcular las expresiones de los 
--  tres primeros apartados del ejercicio
--  anterior, pero utilizando funciones de orden superior

--2.a)
cuadradosOS :: Num a => Int -> [a]

cuadradosOS n = take (n+1) (map (^2) (iterate (+1) 0))


--2.b)
infixl 6 -.
(a,b) -.(c,d) = (a-c,b-d)

eleva (a,b) = (a,b^2)

cuadradosEmparejadosOS :: Int -> [(Int, Int)]

cuadradosEmparejadosOS n =  take (n+1) (map (eleva) (iterate (-.(1,1)) (n,n)))


--2.c)
sumaCosOS :: Floating a => Int -> a

sumaCosOS n = sum (take n (zipWith (*) (iterate (+1) 1) (map abs  (map cos (iterate (+1) 1)))))



--3)Funciones de orden superior, utilizando funciones de orden superior
--  predefinidas en Haskell.
--3.a)
iguales:: (Eq a1, Num a2, Enum a2) =>(a2 -> a1) -> (a2 -> a1) -> a2 -> a2 -> Bool
--iguales f g n m = all (\x -> f x == g x) (take (m+1-n) (iterate (+1) n))

iguales f g n m = all (\x -> f x == g x) ([n,n+1..m])


--3.b)
menorA::(Num a,Enum a)=> a -> a -> (a -> Bool) -> a

--Solo funciona si existe uno que lo cumpla

menorA n m p = head ( filter p [n,n+1..m])


--3.c)
mayor::(Num a,Enum a)=> a -> (a -> Bool) -> a

mayor n p = head (filter p (iterate (+(-1)) n))


--3.d)
ex::(Num a,Enum a)=>a-> a -> (a -> Bool) -> Bool

ex n m p = any p [n,n+1..m]



--4)Programa las siguientes funciones de orden superior, 
--  utilizando funciones de orden superior predefinidas en Haskell:
--  Los tipos de las variables n y m usadas en estas funciones
--  tienen que estar en la clase Enum.

--4.a)
filter2:: [a] -> (a -> Bool) -> (a -> Bool)-> ([a], [a])

filter2 xs p q = (filter p xs, filter q xs)


--4.b)
filters:: [a] -> [(a -> Bool)] -> [[a]]

filters xs ps = map (\x -> filter x xs) ps