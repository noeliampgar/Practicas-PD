-- Programación Declarativa
-- Sesión de laboratorio 4
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

--------------------------------------------------------------------------------------


-- 2)

data Nat = Cero | Suc Nat deriving (Eq, Ord)

infixl 6 +.
(+.):: Nat -> Nat -> Nat

Cero +. x = x
Suc x +. y = Suc(x +. y)

infixl 7 *.
(*.):: Nat -> Nat -> Nat
Suc(Cero) *. x = x
Suc x *. y = y +. x *. y

natToInt:: Nat -> Int

natToInt Cero = 0
natToInt (Suc x) = 1 + natToInt x

instance Show Nat where
 show x = show(natToInt x)

--------------------------------------------------------------------------------------

-- 3)

data Complejo = C (Float, Float) deriving Eq

infix 6 +..
(+..):: Complejo -> Complejo -> Complejo

C (re1,im1) +.. C (re2,im2) = C (re1+re2,im1+im2)

infix 7 *..
(*..):: Complejo -> Complejo -> Complejo

C(re1,im1) *.. C(re2,im2) = C (re1*re2-im1*im2, re1*im2 + re2* im1)

infix 6 -..
(-..):: Complejo -> Complejo -> Complejo

C (re1,im1) -.. C (re2,im2) = C (re1-re2,im1-im2)

instance Num Complejo where
 (+) = (+..)
 (*) = (*..)
 (-) = (-..)
 
instance Show Complejo where
 show (C (re,im))
  | im > 0 = show (re) ++ " + " ++ show (im) ++ "i"
  | im < 0 = show (re) ++ " - " ++ show (negate(im)) ++ "i"
  | im ==0 = show (re)



--------------------------------------------------------------------------------------

-- 4)

class Medible a where
 medida:: a ->Int



instance Medible Bool where
 medida True = 1
 medida False= 0

instance Medible a => Medible [a] where
 medida [] =0
 medida (y:xs) = medida y + medida xs
 
instance (Medible a, Medible b)=>Medible (a,b) where
 medida (a,b) = medida a + medida b
