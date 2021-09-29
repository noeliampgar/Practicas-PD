--Noelia Pérez García-Consuegra, 4º DG

--1)
--1.a)
last1 :: [a] -> a
last1 = foldl (\_ x -> x) undefined

--1.b)
reverse1 :: [a] -> [a]
reverse1  = foldr (\ x xs -> xs ++ [x]) [] 

--1.c)
all1::(a -> Bool) -> [a] -> Bool
all1 p  =foldr ( \ x xs -> (p x) && xs ) (True) 

--1.d)
minimum1 :: Ord a => [a] -> a
minimum1 xs= foldr ( \x y -> min x y ) (head xs) xs

--1.e)
map1:: (a -> b) -> [a] -> [b]
map1 f = foldr (\x xs -> f x:xs) []

--1.f)
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\x xs -> if (p x) then x:xs else xs) []

--1.g)
takeWhile1:: (a -> Bool) -> [a] -> [a]
takeWhile1 p xs = foldr (\x y -> if p x then x:y else []) [] xs --Devuelve lista vacía hasta 
                                                                --q lo complen todos los elementos
                                                                --desde i hasta 0

---------------------------------------------------------------------------------------------------------------------

--2)
--2.a)
foldr11:: (a -> a -> a) -> [a] -> a
foldr11 f [x] = x
foldr11 f (x:xs) =  f x (foldr11 f xs)

--2.b)
foldl11:: (a -> a -> a) -> [a] -> a
foldl11 f [x] = x
foldl11 f (x:y:xs) = foldl11 f (f x y : xs)

---------------------------------------------------------------------------------------------------------------------

--3)
--3.a)
listaRepSignoOp :: Integral a => [a]
listaRepSignoOp = concat [ [n, -n] | n <- [1..]]

--3.b)
listaNXN ::Integral a => [(a,a)]
listaNXN  =[(x,y) | cota <-[0..], x <- [0..cota], y <- [0..cota], x + y == cota ]

---------------------------------------------------------------------------------------------------------------------

--4)
--4.a)sufijos xs = lista de todos los sufijos de xs.
sufijos:: [a] -> [[a]]
sufijos xs = [drop n xs | n<-[0..length xs]]

--4.b)sublistas xs = lista de todas las sublistas de xs.
sublistas :: [a] -> [[a]]
sublistas xs = []: [(take m .drop n) xs |m <-[1..length xs ], n <- [0..length xs -m]]

--4.c)permutaciones xs = lista de todas las permutaciones de xs.
permutaciones:: Eq a => [a]-> [[a]]
permutaciones [x] = [[x]]
permutaciones xs = [x:y | x <- xs, y <- permutaciones(filter(/=x) xs) ]

--4.d)sumandos n devuelve la lista de todas las descomposiciones en sumandos positivos de n.
sumandos :: (Num a, Enum a, Eq a) => a -> [[a]]
sumandos 0 = [[]]
sumandos n = [x:ys | x <- [1..n], ys <- sumandos(n-x)]