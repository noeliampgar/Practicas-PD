--Noelia de las Mercedes Pérez García-Consuegra

--1)

sToYears :: Fractional a => a -> a
sToYears n = n /(365*24*60*60)

--Ejecutamos:
--sToYears (10^6)
--Cuantos años son 10^6 segundos?
--3.1709791983764585e-2

--Ejecutamos:
--truncate(sToYears (10^6))
--Cuantos años enteros son 10^6 segundos?
--0


var1=sToYears (10^6)-fromIntegral(truncate(sToYears (10^6)))
--Mantisa de sToYears (10^6):
--3.1709791983764585e-2


yearsToDias:: Num a => a -> a
yearsToDias n = n*365

--Ejecutamos:
--truncate(yearsToDias(var1))
--Días restantes enteros:
--11


var2=yearsToDias(var1) - fromIntegral(truncate(yearsToDias(var1)))
--Mantisa de yearsToDias(var1):
--0.5740740740740726


diasToHoras :: Num a => a -> a
diasToHoras n = n*24

--Ejecutamos:
--truncate(diasToHoras (var2))
--Horas restantes enteras:
--13


var3=diasToHoras (var2) - fromIntegral(truncate(diasToHoras (var2)))
--Mantisa de diasToHoras (var2)
--0.777777777777743


horasToMinutos :: Num a => a -> a
horasToMinutos n = n*60

--Ejecutamos:
--truncate(horasToMinutos (var3))
--Minutos restantes enteros:
--46


var4=horasToMinutos(var3) - fromIntegral(truncate(horasToMinutos(var3) ))
--Mantisa de horasToMinutos(var3) 
--0.6666666666645824
 

minutosToSecs:: Num a => a -> a
minutosToSecs n = horasToMinutos n

--Ejecutamos:
--truncate(minutosToSecs  (var4))
--Segundos restantes enteros:
--39



--2)

--mediaArit :: Fractional a => [a] -> a
mediaArit l = sum l / (fromIntegral(length l))

--Surge un error de tipo
--Al poner fromIntegral se resuelve
--MAL porque si length es 0 NaN



--3)

fac n 
 | n==0         = 1
 | otherwise = n*fac (n-1)

--Calcular el numero de digitos de un numero entero
nDAux :: (Eq a,Integral a) => a->a->a
nDAux n nD= if (n==0) then nD 
                      else nDAux (div n 10) (nD+1)
nD :: Integral a => a -> a
nD n = nDAux n 0

--comb n m = numero de combinaciones 
--de n elementos tomados de m en m

comb :: Integral p => p -> p -> p
comb n m =div (fac n ) ((fac m) * (fac (n-m) ))

--reduccion x = resultado del proceso de sumar los digitos del entero x, 
--sumar los digitos del resultado obtenido, y asi sucesivamente hasta 
--obtener un numero menor que 10. La reduccion de un entero negativo
-- es la de su valor absoluto.´

sumaDAux :: (Eq a,Integral a) => a->a->a
sumaDAux n sumaD = if (n==0) then sumaD 
                             else sumaDAux (div n 10) (sumaD + (mod n 10))
sumaD n = sumaDAux n 0

reduccion :: Integral a => a -> a
reduccion n= if( n<0) then reduccion (abs n) else
             if(n<10) then n                  
                      else reduccion (sumaD n)



--4)

--Estricta solo en el primer argumento e infija
infixr 2 &&& -- conjuncion
True      &&& y = y
False     &&& _ = False

--Prefijas

--Estricta solo en el primer argumento
and3 True y  = y
and3 False _ = False

--Estricta en el primer argumento y en el segundo
and4 True  True  = True
and4 True  False = False
and4 False False = False
and4 False True  = True

--Estricta solo en el segundo argumento
and5 y True  = y
and5 _ False = False

 

--5)

--Evaluaciones
--last [1..1000] bastante 1.7*10^3/10^8= 1.7/10*5 (en las transparencias aparece un ejemplo con last)
--last [1..10^20] mucho 1.7*10^20/10^8=1.7*10^12 tendría que evaluar del principio al final
--head [1..10^20] poco, por evaluación perezosa
--last [10^20..1] mucho 1.7*10^20/10^8=1.7*10^12
--head (tail [1..10^20]) poco
--length [1..10^20] mucho 1.7*10^20/10^8=1.7*10^12
--last (take (10^7) [1..10^20]) bastante
--head (take (10^7) ([1..100] ++ [1..10^20])) poco
--last (take 100 ([1..10^20] ++ [1..100])) bastante
--last (drop 100 ([1..10^20] ++ [1..100])) bastante
--head (drop (10^7) ([1..10^20] ++ [1..100])) mucho
--[1..10^20]==[1..10^20] mucho
--[1..10^20]==[1..10^20+1] mucho
--[1..10^20]==[2..10^20] poco
--head (reverse [1..10^7]) bastante
--last (reverse [1..10^7]) bastante
--reverse [1..10^20] == reverse [1..10^20+1] mucho


--Algunas pruebas en el main
main=do
 print("EJERCICIO 1:")
 print("Cuantos anyos son 10^6 segundos:")
 print (sToYears (10^6))
 print("Cuantos anyos enteros son 10^6 segundos:")
 print (truncate(sToYears (10^6)))
 print("Dias restantes enteros:")
 print (truncate(yearsToDias(var1)))
 print("Horas restantes enteras:")
 print (truncate(diasToHoras (var2)))
 print("Minutos restantes enteros:")
 print (truncate(horasToMinutos (var3)))
 print("Segundos restantes enteros:")
 print (truncate(minutosToSecs  (var4)))
 print("EJERCICIO 2:")
 print ("Hemos implementado la funcion mediaArit ayudandonos de fromIntegral")
 print([1,2,3,4,5])
 print (mediaArit [1,2,3,4,5])
 print("EJERCICIO 3:")
 print (1234)
 print("Calculamos su numero de digitos")
 print (nD 1234)
 print ("Calculamos su reduccion")
 print (reduccion 1234)
 print ("Calculo el numero de combinaciones de 5 elementos tomados de 3 en 3")
 print (comb 5 3)
 






