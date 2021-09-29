-- Noelia de las Mercedes Pérez García-Consuegra
-- PROGRAMACIÓN DECLARATIVA
-- PRÁCTICA FINAL Curso 2020/2021

-- Objetivo de la practica: Escribir un programa en Haskell que implemente una
-- serie de funciones para trabajar con relaciones binarias sobre un conjunto cualquiera.

------------------------------------------------------------------------------------------------------------------

--CLASE CONJUNTO (se utiliza para representar los conjuntos cociente)

data Cj a = Cj [a]
 deriving Show

creaCjVacio :: Cj a
creaCjVacio = Cj []

esCjVacio :: Eq a => Cj a -> Bool
esCjVacio (Cj []) = True
esCjVacio _       = False

incluirElem :: Eq a => a -> Cj a -> Cj a
incluirElem x c =
   case c of Cj [] -> Cj [x]
             Cj xs -> if elem x xs then c else Cj (x:xs)

conjTolist :: Eq a => Cj a -> [a]
conjTolist (Cj xs) = ys where Cj ys = foldr incluirElem creaCjVacio xs

unirCj:: Eq a => Cj a ->Cj a-> Cj a
unirCj (Cj (x:xs)) c = if xs==[] then incluirElem x c 
                               else unirCj (Cj xs) (incluirElem x c)




subconjunto:: Eq a => Cj a -> Cj a -> Bool
subconjunto c1 c2 = 
   let {xs1 = conjTolist c1; xs2 = conjTolist c2 }  in contenida xs1 xs2
   where contenida xs ys = foldl (\ y x -> (elem x ys) && y) True xs
                                                                  
instance Eq a => Eq (Cj a)
 where c1 == c2 = subconjunto c1 c2 && subconjunto c2 c1

instance Ord a => Ord (Cj a)
 where c1 <= c2 = subconjunto c1 c2

------------------------------------------------------------------------------------------------------------------

-- 1. Define la siguiente estructura de datos para representar en Haskell 
-- este tipo de relaciones: data Rel a = R [(a,a)]
-- Define una funcion esRelacion r que vale True si r es una relacion, 
-- es decir, la lista que representa el conjunto de pares no tiene 
-- repetidos; False en caso contrario.

data Rel a = R[(a,a)] deriving (Read,Show)

esRelacion:: Eq a=> Rel a -> Bool
esRelacion (R [])     = True
esRelacion (R (x:xs)) = not (elem x xs) && esRelacion (R xs)

------------------------------------------------------------------------------------------------------------------

-- 2. El tipo Rel debe ser instancia de las clases Read y Show. 
-- También tiene que ser instancia de la clase
-- Eq, en este caso hay que redefinir los métodos de la clase para 
-- que la igualdad en Rel coincida con
-- la igualdad conjuntista.

elemR :: Eq a => (a,a) -> Rel a -> Bool
elemR x (R c) = any ( == x ) c

instance  (Eq a) => Eq (Rel a) where
 (R [])    == (R [])     = True
 (R [])    == (R (_:_))  = False
 (R (_:_)) == (R [])     = False
 (R (x:xs))== (R (y:ys)) = (elemR x (R (y:ys)) && (R xs) == (R (filter (/=x) (y:ys))))

------------------------------------------------------------------------------------------------------------------

-- 3. Programa en Haskell las siguientes funciones para trabajar con expresiones del tipo Rel a. Puedes
-- dar por hecho que los argumentos de las funciones son realmente relaciones, o mejor aún, puedes
-- hacer una comprobación previa. No olvides declarar el tipo de todas las funciones que definas,
-- incluidos los de las funciones locales:

-- dominio r = conjunto dominio de la relación r.
incluirElemNoRep :: Eq a => (a,a) -> [a] -> [a]
incluirElemNoRep x c = if c == []        then [fst x]  else
                       if elem (fst x) c then c    
                                         else (fst x):c
                          
dominio:: Eq a=> Rel a-> [a]
dominio(R xs) =  foldr incluirElemNoRep [] xs

-- soporte r = conjunto sobre el que esta definida la relación r.
incluirParNoRep :: Eq a => (a,a) -> [a] -> [a]
incluirParNoRep (x,y) c = if y==x then if c == []  then [x]  else
                                   if elem x c then c    
                                               else x:c
                              else if c == []                then [x,y]  else
                                   if elem x c  &&  elem y c then c      else   
                                   if elem x c  then              y:c    else
                                   if elem y c  then              x:c 
                                                else              x:y:c

soporte:: Eq a=> Rel a-> [a]
soporte(R xs) =  foldr incluirParNoRep [] xs

-- relEquivalencia r = True si r es una relación de equivalencia; False en caso contrario

incluirSnd :: Eq a => (a,a) -> [a] -> [a]
incluirSnd x c = if c == []        then [snd x]  else
                       if elem (snd x) c then c    
                                         else (snd x):c
                          
relConX:: Eq a=> Rel a-> a -> [a]
relConX(R xs)  x =  foldr incluirSnd [] ((filter (\(a,b) -> a==x)) xs)

transitivaElem:: Eq a=>Rel a-> a-> Bool
transitivaElem c x = all (\y->elem x  (relConX c y )) (relConX c x)

transitiva :: Eq a=>Rel a-> Bool
transitiva r = all (transitivaElem r) (dominio r)

simetrica:: Eq a=> Rel a ->Bool
simetrica (R xs) = all ((\ys x-> elem (snd x,fst x) ys) xs) xs
reflexiva:: Eq a=> Rel a ->Bool
reflexiva (R xs) = all (( \ys x-> elem (x,x) ys ) xs) (soporte (R xs))

relEquivalencia:: Eq a => Rel a-> Bool
relEquivalencia (R xs) = reflexiva (R xs) && simetrica (R xs) && transitiva (R xs)

-- conjCociente r = conjunto cociente de la relacion r, si esta es relacion de equivalencia.
-- Escribe un mensaje de error, en caso contrario.
 
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion xs ys =
  [x | x <- xs, x `elem` ys]

intersecan :: Eq a => [a] -> [a] -> Bool
intersecan xs ys = length (interseccion xs ys)>0

noIntersecan :: Eq a => [a] -> [a] -> Bool
noIntersecan xs ys = length (interseccion xs ys)==0

incluirElemCjListas :: Eq a => [a] -> Cj [a] -> Cj [a]
incluirElemCjListas x c  =
   case c of Cj [] -> Cj [x]
             Cj xs -> if (any (intersecan x) xs) then  incluirElem  (conjTolist (unirCj (Cj (head (filter(intersecan x) xs)))(Cj x))) ( Cj( filter (noIntersecan x) xs))  else Cj (x:xs)

--(conjTolist( unirCj (Cj(head(filter(intersecan x) xs))) x) )

cjtoCocienteAux:: Eq a => Rel a -> Cj [a]
cjtoCocienteAux (R [])      = Cj [] 
cjtoCocienteAux (R (x:xs))  = incluirElemCjListas  (claseEq (R (x:xs)) (fst x)) (cjtoCocienteAux (R xs))
                                                         
cjtoCociente r = if (relEquivalencia r) then (cjtoCocienteAux r ) else error "No es una relación de equivalencia\n"
claseEq r x = relConX r x


-- generaDiv n m = r donde r es la relacion {(x, y) | n <= x, y <= m, x es divisor de y}

divisores :: Integral a => a -> [a]
divisores y = [x | x<-[1..y], mod y x ==0]

generaDiv :: Integral b => b -> b -> Rel b
generaDiv n m = R [(x, y) |  y<-[1..m],x<-(divisores y),n <= x]

-- generaGE xs = r donde r es la relación >= sobre el conjunto de elementos de la lista xs.

generaGE:: Ord a=> [a] -> Rel a
generaGE xs = R[(x, y) |  x<-xs,y<-xs, x>=y]

-- composicion r1 r2 = r1 o r2. Observa que las relaciones r1 y r2 tienen que estar definidas
-- sobre el mismo conjunto.

listToRel:: Eq a => [(a,a)] ->Rel a
listToRel  xs = (R ys) where  ys = foldr (\z zs-> if (elem z zs) then zs else z:zs) [] xs

composicion :: Eq a => Rel a -> Rel a -> Rel a
composicion (R (xs)) (R (ys)) = listToRel ( [(x,y) | (x,u) <- xs, (v,y) <- ys, u == v])

-----------------------------------------------------------------------------------------------------------

-- RELACIONES PREDEFINIDAS:

-- No equivalencia
r1:: Integral a => Rel a
r1 = R [(2,2),(3,2),(3,3),(4,2),(4,3),(4,4),(5,2),(5,3),(5,4),(5,5),(6,2),(6,3),(6,4),(6,5),(6,6)]

-- De equivalencia
r2:: Integral a => Rel a
r2 = R [(4,4),(4,5),(5,4),(5,5),(0,0)]

-- Isomorfa a la anterior (de equivalencia)
r3:: Integral a => Rel [a]
r3 = R [([1,2],[1,2]),([1,2],[3,4]),([3,4],[1,2]),([3,4],[3,4]),([7,8],[7,8])]

-- Conjunto Z2 
r4:: Integral a =>Rel a
r4 = R [(0,0),(1,1)]

-----------------------------------------------------------------------------------------------------------

-- 4. Implementa las siguientes acciones de E/S, declarando tipos adecuados:

-- introRel lee una relación introducida por el usuario. Debe pedir al usuario que introduzca los
-- pares de la relación uno a uno.

getPair:: IO (Int,Int)
getPair = do line <- getLine
             return (read line::(Int,Int))


leeRelacion::Rel Int -> IO (Rel Int)
leeRelacion  (R xs) =
 do putStr("Introduce la siguiente relación -(-1,-1) para acabar)-\n") 
    line <- getPair
    if( line == (-1,-1)) then return (R xs)
                         else leeRelacion  (R (line:xs))

-- Se presupone que el usuario no introduce pares repetidos
introRel:: IO (Rel Int) 
introRel = leeRelacion (R [])

-- muestraRel invoca a la función introRel y muestra la relación introducida en forma de tabla.
-- La relación R1 definida al principio se podría mostrar en forma de tabla.

dibujaVector :: [Int] -> IO()
dibujaVector [] = do putStr ""
dibujaVector v = do putStrLn (unwords [show (x) | x <- v])

dibujaMatriz :: [[Int]] -> IO()
dibujaMatriz [] = do putStr ""
dibujaMatriz (v:m) = do dibujaVector v
                        dibujaMatriz m
boolToInt:: Bool ->Int
boolToInt True  = 1
boolToInt False = 0

matrizDeRel :: Eq a => Rel a -> [[Int]]
matrizDeRel  r=[[boolToInt(elem y (relConX r x))| y<-soporte r] | x<-soporte r]

muestraRel :: Eq a => Rel a -> IO()
muestraRel r = dibujaMatriz (matrizDeRel r)