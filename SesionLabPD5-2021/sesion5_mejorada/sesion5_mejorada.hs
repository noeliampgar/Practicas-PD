-- Programación Declarativa
-- Sesión de laboratorio 5
-- Curso 2020/21

-- Noelia Pérez García-Consuegra

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Funciones auxiliares:
getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- 1)

adivina :: Int -> IO ()
adivina n = do print ("Introduce un numero:")
               x <- getInt
               if x == n then do print "Has adivinado!"
                         else if x < n then do print "El numero buscado es mayor"
                                               adivina n
                                        else do print "El numero buscado es menor"
                                                adivina n

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--2)
formatea :: String -> String -> Int -> IO ()

formatea fileIn fileOut n = do 
                              strFileIn <- readFile fileIn                                  -- strFileIn guarda todo el texto origen como String
                              let
                                listFileIn = lines strFileIn                                -- listFileIn es una lista de las lineas que en strFileIn van separadas por '\n'
                                listFileIn' = format listFileIn n                           -- Ahora tenemos una lista de las lineas como en listFileIn pero con un número de espacios adecuado entre las palabras de cada linea
                                strListFileIn'= unlines listFileIn'                         -- Tenemos una lista de String y unline la convierte en un String separando cada elemento por un salto de linea
                                in
                                  writeFile fileOut strListFileIn'                          -- Lo escribo en el fich de salida
                                    

format :: [String] -> Int -> [String]

format listFileIn n = let
                  listWrFileIn = map words listFileIn                                       -- Lista de listas de String (cada lista de String está formada por las palabras de cada línea que iban separadas por espacios)
                                                                                             
                  listLongLines = map (length . concat) listWrFileIn                        -- Lista de las longitudes de las líneas del texto sin contar los espacios
                  listWrFileIn' = [putSpaces n long listPalabras | (long, listPalabras) <- zip listLongLines listWrFileIn] 
                                                                                            -- long es la longitud de una linea concreta sin espacios y listPalabras es la lista de palabras separadas por espacios de esa linea
                                                                                            -- Es una lista como  listWrFileIn, pero  con cada lista de String con espacios añadidos a los String
                  in
                    map concat listWrFileIn'


putSpaces :: Int -> Int -> [String] -> [String]

putSpaces n long listPalabras = if long > n || null listPalabras then                      -- Si la longitud de una linea es mayor que n o la linea era vacía devuelvo listPalabras de la linea
                    listPalabras
                   else                                                                    -- Si no devuelve la lista de palabras con los espacios correspondientes en cada palabra para que puedan ser unidas y que la linea mida n caracteres
                    let
                      numHuecos = length listPalabras - 1
                      nMenosPalabras = n - long
                      numHEntrePal = div nMenosPalabras numHuecos
                      r = mod nMenosPalabras numHuecos                                     -- Para añadir algún hueco extra entre las primeras palabras
                      in
                        [word ++ replicate numHEntrePal ' ' ++ (\i -> if i > r then [] else " ") i | (word, i) <- zip listPalabras [1..], i <= numHuecos] ++ [last listPalabras]  --i menor o igual que numHuecos para luego añadir la palabra final sin huecos al final
                                                                                           -- replicates crea una lista con el argumento 2 repetido argumento 1 veces
                                                                                           -- Lo que hace es añadir a cada palabra sus huecos correspondientes


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--3)


--3.a)
type Vector = [Float]
type Matriz = [Vector]

transp :: Matriz -> Matriz
transp [] = []
transp [fila] = map (:[]) fila
transp (fila:fs) = zipWith (:) fila (transp fs )

sumaMat :: Matriz -> Matriz -> Matriz
sumaMat m1 m2 = zipWith sumafilas m1 m2
       where sumafilas fl f2 = zipWith (+) fl f2


escalar :: Vector -> Vector -> Float                  --producto escalar usual
escalar us vs = sum $ zipWith (*) us vs

prodVecMat :: Vector -> Matriz -> Vector     
prodVecMat v m = [escalar v fila | fila <- transp m]  --producto vector-matriz

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 =[prodVecMat fila m2 | fila <- m1]      --producto matriz-matriz

--3.b)
dibujaMatriz:: Matriz->IO()

dibujaMatriz m = dibujaMatrizAux m 0


dibujaMatrizAux :: Matriz->Int->IO()

dibujaMatrizAux m n = if n == length m    then putStr ""
                                          else do
                                               putStrLn ((map concat [putSpaces 10 (length (m!!0))   listNums |  listNums <-  map (map show) m]) !!n)
                                               dibujaMatrizAux m (n+1)
