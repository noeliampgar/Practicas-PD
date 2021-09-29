-- Programación Declarativa
-- Sesión de laboratorio 4 bis
-- Curso 2020/21

-- Noelia Pérez García-Consuegra

--------------------------------------------------------------------------------------

-- 1)

type Punto = (Int, Int)

data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (Eq, Ord, Show)

destino:: Punto -> [Direccion] -> Punto

destino p dir = foldl f p dir
 where f (p1,p2) ARRIBA = (p1,p2+1)
       f (p1,p2) ABAJO = (p1,p2-1)
       f (p1,p2) IZQUIERDA = (p1,p2+1)
       f (p1,p2) _ = (p1-1,p2)

-- 1.a)

trayectoria:: Punto -> [Direccion] -> [Punto]

trayectoria p [] = []

trayectoria (c1,c2) (x:xs) --No aparece el punto inicial
 | x == ARRIBA = (c1,c2+1):(trayectoria (c1,c2+1) xs)
 | x == ABAJO = (c1,c2-1):(trayectoria (c1,c2-1) xs) 
 | x == DERECHA = (c1+1,c2):(trayectoria (c1+1,c2) xs)
 | True = (c1-1,c2):(trayectoria (c1-1,c2) xs)

-- 1.b)

(>.):: Punto->Punto->Bool
infixr 2 >.
(a,b) >. (c,d) = b>d

maximum' ::  [Punto] -> Punto
maximum' = foldr1 (\x y ->if x >. y then x else y)

minimum' ::  [Punto] -> Punto
minimum' = foldr1 (\x y ->if y >. x then x else y)

enNN:: Int->Punto -> Bool 
--Función que comprueba si un Punto esta dentro de nxn
enNN n (a,b) = 0<=a && a<=n && 0<=b && b<=n

inferior:: Int->[Punto]->[Punto]->Bool
--También comprueba si la trayectoria no se sale del plano
inferior n movs movs'= all (enNN n) movs && all (enNN n) movs' && 
   (snd (minimum' movs') == snd (maximum' movs) || minimum' movs' >. maximum' movs) 


--------------------------------------------------------------------------------------


-- 2)Define un tipo de datos polimórfico para representar árboles generales, 
-- en los que cada nodo tiene una información y n hijos 
-- (n >= 0, y puede variar con cada nodo). No se consideran árboles vacíos.

data Arbol a = Hoja a | Nodo a [Arbol a] deriving Eq

-- 2.a.1)

listaHojas :: Arbol a -> [a]

listaHojas (Hoja x) = [x]
listaHojas (Nodo x xs) = foldl f [] xs
 where f x y = x ++ listaHojas y

-- 2.a.2)

listaNodos :: Arbol a -> [a]

listaNodos (Hoja x) = [x]
listaNodos (Nodo x xs) = foldl f [x] xs
 where f x y = x ++ listaNodos y

-- 2.a.3)

repMax :: Ord a => Arbol a -> Arbol a

repMax (Hoja x) = Hoja x
repMax (Nodo x xs) = Nodo (max x m2) (foldl f [] xs)
 where m2 = maximum (listaNodos (Nodo x xs))
       f xs (Hoja _) = xs ++ [Hoja (max x m2)]  
       f xs (Nodo z ys) = xs ++ [repMax (Nodo (max x m2) ys)]

-- 2.b)

instance Ord a => Ord (Arbol a) where
 (Hoja _) <= (Nodo _ _) = True
 (Nodo _ _) <= (Hoja _) = False
 (Hoja x) <= (Hoja y)           = x<=y
 (Nodo x xs) <= (Nodo y ys)     = x<=y || x == y && xs <= ys

-- 2.c)

instance Show a => Show (Arbol a) where
 show (Hoja x) = " * " ++ (show x)++" "
 show (Nodo x xs) = " · " ++ (show x) ++" " ++ (show xs)
