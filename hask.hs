doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmall x = if x > 100 then x else doubleMe x

messi :: Int -> [Char]
messi x = if x /= 10 then "messi "++(show x) else "ola soy messi"

f = [1,2,54,1,2,5]
vacio = []

cortar :: Int -> Int -> [Char] -> [Char]
cortar i j w = take (j-i+1) (drop (i-1) w)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Int -> Int -> Int
factorial' n i = if n == 0 then i else factorial' (n-1) (n*i)

fact :: Int -> Int
fact n = factorial' n 1

sinMinus :: [Char] -> [Char]
sinMinus st = [c | c <- st ,c `elem` ['A'..'Z']]

sinMayus :: [Char] -> [Char]
sinMayus st = [c | c <- st ,c `elem` ['a'..'z']]

aMayus :: [Char] -> [Char]
aMayus st = [if esMayus (c)==True then c else convert (c)|c <-st]

aMinus :: [Char] -> [Char]
aMinus st = [if esMayus (c)==False then c else convert (c)|c <-st]

convert :: Char -> Char
convert ' ' = ' '
convert c = if esMayus (c)==True then toEnum( fromEnum (c) + 32) else toEnum( fromEnum (c) - 32)

esMayus :: Char -> Bool
esMayus c = if c `elem` ['A'..'Z'] then True else False

digitos :: Int -> [Int]
digitos x = if x < 10 then [x] else digitos (x `div` 10) ++ [x `mod` 10]

--------------------------------------FIN PRACTICA 1------------------------------------------

--2, Definir hd, tl, last, init
hd :: [a] -> a
hd [] = error "Lista vacía"
hd (a:ab) = a

hd' :: [a] -> a
hd' a = a!!0

tl :: [a] -> [a]
tl (a:ab) = ab

lst :: [a] -> a
lst [x] = x
lst (x:xs) = lst xs

innit :: [a] -> [a]
innit [a] = []
innit (a:ab) = a:innit ab

--3, función máximo de 3
maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)

--4, Función concatenar, tomar, tirar y ':'
concatenar :: [a] -> [a] -> [a]
concatenar [][] = []
concatenar []cd = cd
concatenar a[] = a
concatenar (a:ab) cd = a : concatenar ab cd

primeraLetra :: [Char] -> [Char]
primeraLetra [] = ""
primeraLetra all@(x:xs) = "La primera letra de " ++ all ++ " es " ++ [x]

--5, Función valor absoluto
absoluto :: Int -> Int
absoluto x = if x<0 then -x else x

--f1 :: (Int,Int,Int)
--f2 :: (Int,Int,Int)
f1 = (20,10,1968) :: (Int,Int,Int)
f2 = (30,4,1987) :: (Int,Int,Int)

--6, función edad que devuelve los años transcurridos entre
--dos fechas dadas
edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int
edad (x,y,z) (a,b,c) | b<y || (b == y) && (a < x) = c-z-1
                     |otherwise = c-z

--7, disyunción excluyente 
xor' :: Bool -> Bool -> Bool
xor' p q = if (p== True && q== False) || (p== False && q == True) then True else False

xor :: Bool -> Bool -> Bool
xor True True = False
xor True _ = True
xor _ True = True
xor False False = False

--8, dado un número natural, una función que decida si el
--número es primo o no
divisores :: (Integral a) => a -> [a]
divisores x = [e | e <- [1..x], mod x e == 0]

esPrimo :: (Integral a) => a -> Bool
esPrimo x = divisores x == [1,x]

--9, Una función que dado un número x devuelva una lista con 
--todos los primos menores a n
primosMenores :: Int -> [Int]
primosMenores x = [c | c <- [1..x], esPrimo c]

--10, Una función que dada una lista retorne la reversa de 
--la misma
reversa :: [a] -> [a]
reversa [] = []
reversa (a:ab) = reversa ab ++ [a]

--12, función que dada una lista decina si es un palíndromo 
--o no
palindromo :: (Eq a) => [a] -> Bool
palindromo x = reverse x == x 

--11, Dada una lista de números, devuelve la lista solo con 
--los numeros primos
soloPrimos :: [Int] -> [Int]
soloPrimos st = [c | c <- st, esPrimo c]

--13, Defina una función que dados tres números a, b, c devuelva
--la cantidad de raíces reales de la ecuación ax2 + bx + c = 0
raicesCuad :: Float -> Float -> Float -> Int
raicesCuad a b c |((b*b) - (4*a*c)) > 0 = 2
                 |((b*b) - (4*a*c)) == 0 = 1
                 |otherwise = 0
--------------------------------------FIN PRACTICA 2------------------------------------------
--PRÁCTICO 3

--1, define una función que, dadas dos listas xs y ys de naturales ordenadas, retorne el merge de estas listas, es decir
--la lista ordenada por los elementos de ys y xs

--Funciones de ordenamiento:
--QuickSort
quick :: Ord a => [a] -> [a]
quick [] = []
quick (x:xs) = quick left ++ [x] ++ quick right
    where left = [a | a <- xs, a <= x]
          right = [b | b <- xs, b > x]
--BubbleSort
-- bubble :: Ord a => [a] -> [a]
-- bubble [] = []
-- bubble [x] = [x]
-- bubble (x:y:xs)| x <= y = [x] ++ bubble (y:xs)
               -- | otherwise = [y] ++ bubble (x:xs)

merge :: Ord a => [a] -> [a] -> [a] 
merge [] ys = quick ys
merge xs [] = quick xs
merge xs ys = quick (xs ++ ys)

--Esta versión requiere que las listas estén ordenadas 
--antes de llamar a la función
mergeOrd :: [Int] -> [Int] -> [Int]
mergeOrd [] ys = ys
mergeOrd xs [] = xs
mergeOrd (x:xs) (y:ys) | x < y = [x] ++ mergeOrd xs (y:ys)
                       |otherwise = [y] ++ mergeOrd (x:xs) ys

--Esta función ordena las listas y luego llama a la función
--anterior
merge' :: [Int] -> [Int] -> [Int]
merge' xs ys = mergeOrd (quick xs) (quick ys)

--2 Una función que dada una lista de naturales, la ordene
quickOrd :: Ord a => [a] -> [a]
quickOrd [] = []
quickOrd (x:xs) = quick left ++ [x] ++ quick right
    where left = [a | a <- xs, a <= x]
          right = [b | b <- xs, b > x]

lista1 :: [Int]
lista1 = [2,3143,261346,145]
lista2 :: [Int]
lista2 = [1235,1435,2134,4,134,-555]

--3, define una función que, recursivamente y solo utilizando adición y multiplicación, calcule, dado un natural n, el numero 2^n
dosALa :: Int -> Int
dosALa 0 = 1
dosALa n = 2 * dosALa (n-1)

--4, una función que dado un numero natural n, retorne
--su represesntación binaria como secuencia de bits
aBinario :: Integer -> [Int]
aBinario 0 = [0]
aBinario 1 = [1]
aBinario n = aBinario (fromIntegral (n) `div` 2) ++ [fromIntegral (n) `mod` 2]

numero = 99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
--5, dado un numero natural n en su representación binaria,
--decida si es par o no
binarioPar :: [Int] -> Bool
binarioPar xs = even (last xs)

binarioEven :: [Int] -> Bool
binarioEven [] = error "boludo"
binarioEven [x] = if x == 0 then True else False
binarioEven (x:xs) = binarioEven xs

--6, Define la función que retorne la distancia de Hamming: 
--dadas dos listas es el número de posiciones en que
--los correspondientes elementos son distintos.
distanciaH :: Eq a => [a] -> [a] -> Int
distanciaH [] [] = 0
distanciaH [] (y:ys) = 0--1 + distanciaH [] ys
distanciaH (x:xs) [] = 0--1 + distanciaH xs []
distanciaH (x:xs) (y:ys) | x == y = 0 + distanciaH xs ys
                         | x /= y = 1 + distanciaH xs ys

binarioPar' :: Int -> Bool
binarioPar' n = even (last (aBinario (fromIntegral (n))))
--7, función que dado un numero natural, decida si
--el mismo es un cuadrado perfecto o no
cuadPerfecto :: Int -> Bool
cuadPerfecto n = n `elem` [x^2 | x <- [1..n]]
--cuadPerfecto n = [n | x <- [0..n], x*x==n] /= []

--8, función repetidos de forma tal que dado un elemento
--z y un entero n, z aparece n veces
repetidos :: a -> Int -> [a]
repetidos _ 0 = []
repetidos c n = [c] ++ repetidos c (n-1)


--9, define la función nelem tal que nelem xs n es
--elemento ene-esimo de xs, empezando a numerar desde el 0
nelem :: [a] -> Int -> [a]
nelem [] n = error "sos re estupido"
nelem (x:xs) 0 = [x]
nelem (x:xs) n = nelem xs (n-1)

--10, función posicionesC tal que posicionesC xs c es la
--lista de las posiciones del caracter c en la cadena xs
aparece :: Eq a => [a] -> a -> Int
aparece [] _ = -maxBound
aparece (x:xs) n = if x == n then 
                      0 
                   else
                      1 + aparece xs n

posicionesC :: [Char] -> Char -> [Int]
posicionesC [] c = []
posicionesC lista@(x:xs) c = [4]

-- posicionesC lista@(x:xs) c | c == x = [aparece lista c] ++ posicionesC xs c
                           -- | c /= x = posicionesC xs c

listaPos' :: Eq a => [a] -> a -> Int -> [Int]
listaPos' [] _ _ = []
listaPos' (x:xs) c n = if x == c then n : listaPos' xs c (n+1) else listaPos' xs c (n+1)

listaPos :: Eq a => [a] -> a -> [Int]
listaPos st c = listaPos' st c 0 

--11, función compact, dada una lista retorna la lista sin
--los elementos repetidos consecutivos

compact :: Eq a => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:xs)
        | x == y = compact (x:xs)
        | otherwise = x : compact (y:xs)
--------------------------------------FIN PRACTICA 3------------------------------------------
--CINCO SOLUCIONES CINCO

--factorialLista
factorialLista' :: Int -> [Int]
factorialLista' 0 = [1]
factorialLista' n = [n*fact (n-1)] ++ factorialLista' (n-1)

factorialLista :: Int -> [Int]
factorialLista n = reverse (factorialLista' n)

--factorialGuardas
factorialGuardas :: Int -> Int
factorialGuardas n 
        | n == 0 = 1
        | n == n = n * factorialGuardas (n-1)

--factorialAltoOrden
factorialMap :: Int -> [Int]
factorialMap n = map fact [0..n]

--elem
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) 
        | x == a = True
        | otherwise = myElem a xs
--------------------------------------FIN 5 SOLUCIONES------------------------------------------
--PRÁCTICO 4
--1, Muestra los pasos de reducción hasta llegar a la forma normal de la expresión:
-- 2 * cuadrado (hd [2,4,5,6,7,8])
-- donde cuadrado :: Int -> Int
--       cuadrado x = x*x
--       hd :: [a] -> a
--       hd (x:xs) = x

-- a) Reducción aplicativa:
-- 2 * cuadrado (hd [2,4,5,6,7,8])
-- {Definición de hd}
-- 2 * cuadrado (2)
-- {Definición de cuadrado}
-- 2 * (2 * 2)
-- {Aritmética}
-- 2 * 4
-- {Aritmética}
-- 8

-- b) Reducción normal:
-- 2 * cuadrado (hd [2,4,5,6,7,8])
-- {Definición de cuadrado}
-- 2 * hd [2,4,5,6,7,8,] * hd [2,4,5,6,7,8]
-- {Definición de hd}
-- 2 * 2 * hd [2,4,5,6,7,8]
-- {Aritmética}
-- 4 * hd [2,4,5,6,7,8]
-- {Definición de hd}
-- 4 * 2
-- {Aritmética}
-- 8

--2, Dada la definición: linf = 1 : linf
-- hd linf

-- a) Reducción aplicativa:
-- hd linf 
-- {Definición de linf}
-- hd (1:linf)
-- {Definición de linf}
-- hd (1:1:linf)
-- {Definición de linf}
-- hd (1:1:1:linf)
-- ...
-- No termina nunca

-- b) Reducción normal:
-- hd linf
-- {Definición de linf}
-- hd (1:linf)
-- (Definición de hd)
-- 1

-- 3, Dada la sig definición:
-- f :: Int -> Int -> Int
-- f x 0 = x
-- f x (n+1) = cuadrado (f x n)

-- a) Orden de reducción aplicativo:
-- f 2 3
-- {Definición de f}
-- cuadrado (f 2 2)
-- {Definición de f}
-- cuadrado (cuadrado (f 2 1))
-- {Definición de f}
-- cuadrado (cuadrado (cuadrado (f 2 0)))
-- {Definición de f}
-- cuadrado (cuadrado (cuadrado (2)))
-- {Definición de cuadrado}
-- cuadrado (cuadrado (2 * 2))
-- {Aritmética}
-- cuadrado (cuadrado 4)
-- {Definición de cuadrado}
-- cuadrado (4 * 4)
-- {Aritmética}
-- cuadrado 16
-- {Definición de cuadrado}
-- 16 * 16
-- {Aritmética}
-- 256

-- b) Orden de reducción normal
-- f 2 3 
-- {Definición de f}
-- cuadrado (f 2 2)
-- {Definición de cuadrado}
-- (f 2 2) * (f 2 2)
-- {Definición de f}
-- (cuadrado (f 2 1)) * (f 2 2)
-- {Definción de cuadrado}
-- (f 2 1) * (f 2 1) * (f 2 2)
-- {Definición de f}
-- cuadrado (f 2 0) * (f 2 1) * (f 2 2)
-- {Definición de cuadrado}
-- (f 2 0) * (f 2 0) * (f 2 1) * (f 2 2)
-- {Definición de f}
-- 2 * (f 2 0) * (f 2 1) * (f 2 2)
-- {Definción de f}
-- 2 * 2 * (f 2 1) * (f 2 2)
-- {Aritmética}
-- 4 * (f 2 1) * (f 2 2)
-- {Definición de f}
-- 4 * cuadrado (f 2 0) * (f 2 2)
-- {Definición de cuadrado}
-- 4 * (f 2 0) * (f 2 0) * (f 2 2)
-- {Definición de f}
-- 4 * 2 * (f 2 0) * (f 2 2)
-- {Aritmética}
-- 8 * (f 2 0) * (f 2 2)
-- {Definición de f}
-- 8 * 2 * (f 2 2)
-- {Aritmética}
-- 16 * (f 2 2)
-- {Definición de f}
-- 16 * cuadrado (f 2 1)
-- {Definción de cuadrado}
-- 16 * (f 2 1) * (f 2 1)
-- {Definición de f}
-- 16 * cuadrado (f 2 0) * (f 2 1)
-- {Definición de cuadrado}
-- 16 * (f 2 0) * (f 2 0) * (f 2 1)
-- {Definición de f}
-- 16 * 2 * (f 2 0) * (f 2 1)
-- {Aritmética}
-- 32 * (f 2 0) * (f 2 1)
-- {Definición de f}
-- 32 * 2 * (f 2 1)
-- {Aritmética}
-- 64 * (f 2 1)
-- {Definición de f}
-- 64 * cuadrado (f 2 0)
-- {Definición de cuadrado}
-- 64 * (f 2 0) * (f 2 0)
-- {Definición de f}inf 						
-- 64 * 2 * (f 2 0)
-- {Aritmética}
-- 128 * (f 2 0)
-- {Definición de f}
-- 128 * 2
-- {Aritmética}
-- 256 

-- 4, Utilizando orden aplicativo y normal, evalua la siguiente expresión: square inf
-- considerando las siguientes definiciones:

-- square :: Int -> Int
-- square x = x * x

-- inf :: Int
-- inf = inf + 1

-- Orden de reducción aplicativo:
-- square inf
-- {Definición de inf}
-- square (inf + 1)
-- {Definición de inf}
-- square ((inf + 1) + 1)
-- {Definición de inf}
-- square ((inf + 1) + 1) + 1)
-- ...

-- Orden de reducción normal
-- square inf
-- {Definición de square}
-- inf * inf
-- {Definición de inf}
-- (inf + 1) * inf
-- {Definición de inf}
-- ((inf + 1) + 1) * inf
-- ...

-- 5, ejercicio 3 con reducción lazy
-- 

-- 6, ¿Se puede cambiar el orden de evaluación en Haskell? 
-- ¿Para qué puede servir hacerlo? Dar un ejemplo en el 
-- cual sea útil hacerlo

--------------------------------------FIN PRÁCTICA 4------------------------------------------
-- PRÁCTICA 5
-- 1, Genera una lista infinita de unos
infX :: Int -> [Int]
infX x = [x,x..]
-- infX x = x : infX x

-- 2, Genera una lista infinita de naturales comenzando desde un número dado
infFrom :: Int -> [Int]
infFrom x = [x,(x+1)..]
-- infFrom x = x : infFrom (x+1)

-- 3, Generar una lista con los primeros n naturales
primerosN :: (Integral a) => a -> [a]
primerosN n = [1..n]

-- 4, Retornar los primeros 5 elementos de una lista infinita de enteros positivos
prim5 :: [Int] -> [Int]
prim5 (x:y:z:a:b:xs) = [x,y,z,a,b]

-- UTILIZANDO FUNCIONES DE ALTO ORDEN
-- 5, Dada una lista de enteros, returnar sus cuadrados
square :: (Num a) => a -> a
square x = x*x

listCuad :: (Num a) => [a] -> [a]
listCuad xs = map (square) xs

-- 6, Dado un entero positivo, retornar la lista de sus divisores
esDiv :: Int -> Int -> Bool
esDiv a b = mod b a == 0

divisoresN :: Int -> [Int]
divisoresN n = filter (\x -> esDiv x n) list
        where list = [1..n]

-- 7, Dada una lista de naturales, obtener la lista que contenga solo los
-- números primos de la lista original
onlyPrimos :: (Integral a) => [a] -> [a]
onlyPrimos xs  = filter (\n -> esPrimo n) xs

-- 8, Dada una lista de naturales, retornar la suma de los cuadrados de la lista
sumCuadrados :: (Integral a) => [a] -> a
sumCuadrados xs = (foldl (+) 0 (listCuad xs))

-- 9, Dada una lista de naturales, retornar la lista con sus sucesores
listSucc :: (Integral a) => [a] -> [a]
listSucc xs = map (+1) xs

-- 10, Dada una lista de enteros, sumar todos sus elementos
listSum :: (Integral a) => [a] -> a
listSum xs = foldl (+) 0 xs

-- 11, Definir el factorial usando fold
factFold :: (Integral a) => a -> a
factFold n = foldl (*) 1 [1..n]

-- 12, Redefinir la función and tal que and xs se verifica si todos los
-- elementos de xs son verdaderos
andCoso :: [Bool] -> Bool
andCoso xs = foldl (&&) True xs

-- 13, Usando foldl o foldr definir una función tam::[a]->Int
-- que devuelve la cantidad de elementos de una lista dada 
-- tam :: (Num a) => [a] -> Int
-- tam xs = (foldr (+(\n -> (n/n))) 0 xs)

-- función que devuelve una lista con n unos.
nUnos :: Int -> [Int]
nUnos 0 = []
nUnos n = 1 : nUnos (n-1)

-- Parcialito
-- 1, Definir la función deleteN :: Eq a => a -> [a] -> [a], de forma tal que 
-- (deleteN n x xs) es la lista obtenida luego de eliminar las primeras n 
-- ocurrencias de x en xs. Ej, (deleteN 2 a salamanca) --> slmanca

deleteN :: Eq a => Int -> a -> [a] -> [a]
deleteN 0 _ xs = xs
deleteN _ _ [] = []
deleteN n a (x:xs) | (a==x) = deleteN (n-1) a xs
                   | (a/=x) = x : deleteN n a xs

-- coso
-- 2, definir por comprensión la función allOcurIn :: Eq a => [a] -> [a] -> Bool
-- de forma tal que (allOcurIn xs ys) se verifica si todos los elementos de xs 
-- son elementos de ys
-- Por ejemplo, (allOcurIn [1,5,2,5] [5,1,2,4]) --> True
-- (allOcurIn [1,5,2,5] [5,2,4]) --> False

allOcurIn :: Eq a => [a] -> [a] -> Bool
allOcurIn [] _ = True
allOcurIn xs [] = False
allOcurIn xs ys = (length [a | a <- xs, a `elem` ys]) == (length xs)

-- USANDO LISTAS POR COMPRENSIÓN
-- 14, Dada una lista de enteros, retornar sus sucesores
listaSucc :: [Int] -> [Int]
listaSucc [] = []
listaSucc xs = [x+1 | x <- xs]

-- 15, Dada una lista de naturales, retornar sus cuadrados
listaCuad :: [Int] -> [Int]
listaCuad [] = []
listaCuad xs = [x*x | x <- xs]

-- 16, Dada una lista de enteros, retornar los elementos
-- pares que sean mayores a 10
parMay10 :: [Int] -> [Int]
parMay10 [] = []
parMay10 xs = [x | x <- xs, x > 10, mod x 2 == 0]

-- 17, Dado un entero retornar sus divisores
-- divisoresComp :: Int -> [Int]
-- divisoresComp n = [a |]

-- 18, 

-- 19, dado un natural n, retornar los numeros primos entre 2 y n

-- 20, dadas dos listas de naturales, retornar su producto cartesiano
prodCartesiano :: [Int] -> [Int] -> [(Int, Int)]
prodCartesiano xs ys = [(x,y) | x <- xs, y <- ys]

-- 21, Dadas una lista y un elemento retornar el numero de ocurrencias
-- de x en la lista ys


-- 22, escribir la función split2 :: [a] -> [([a], [a])], que dada 
-- una lista xs, devuelve la lista con todas las formas de partir xs
-- en dos. Ej. split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3],[])]

split2 :: [a] -> [([a],[a])]
split2 xs = [(take x xs, drop x xs) | x <- [0..length xs]]

-- 23, Definir una función que dada una lista de enteros, devuelva la suma
-- de la suma de todos los segmentos iniciales
-- Ej. sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10


-- 24, Definir la lista infinita de los numeros pares
-- infPares :: [Int]
-- infPares = [a, ]

--------------------------------------FIN PRÁCTICA 5------------------------------------------
-- PRÁCTICA 6

-- Ejemplo otros tipos definidos
data Colores = Rojo | Verde | Azul
-- data Lista a = Vacía | 	a

-- 2, Definir la función natToInt : Nat -> Int que dado un numero Nat retorna
-- su Int correspondientes
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ x) = 1 + natToInt x

-- 3, Definir la función intToNat : Int -> Nat

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat x = (Succ (intToNat (x-1)))

-- 4, Definir sumaNat : Nat -> Nat -> Nat, suma de Nat
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero (Succ x) = Succ x
sumaNat n m = intToNat ((natToInt n) + (natToInt m))

-- 1, Definir el tipo Nat, visto en el teórico
data Nat = Zero | Succ Nat 

-- 5, Instanciar las clases Eq, Ord y Show para Nat sin utilizar deriving. Averiguar qué
-- orden define Haskell si se utiliza deriving Ord para Nat
instance Show Nat where
            show n = show (natToInt n)
instance Eq Nat where
            (==) Zero Zero = True
            (==) Zero _ = False
            (==) _ Zero = False
            (==) (Succ n) (Succ m) = (n == m)
{-instance Ord Nat where
            (<=) Zero Zero = True
            (<=) Zero (Succ n) = True
            (<=) (Succ n) Zero = True
            (<=) (Succ n) (Succ m) = (n <= m)
            compare Zero Zero = EQ
            compare Zero (Succ n) = LT
            compare (Succ n) Zero = GT
            compare (Succ n) (Succ m) = compare n m-}

instance Ord Nat where
        (<=) Zero Zero = False
        (<=) Zero _ = True
        (<=) _ Zero = False
        (<=) (Succ n) (Succ m) = n < m

-- 6, Definir árboles binarios

-- data Tree a = Nil | Nodo (Tree a) a (Tree a)
-- instance Show a (Tree a) where
            -- show Nil = "{}"
            -- show Nodo a x b = "(" ++ show a ++ show x ++ show b ++ ")"

-- 7, La función size , que dado un árbol retorna el número de nodos del árbol
-- size :: Tree -> Int
-- size Tree Nil = 0
-- size Tree a = 1 + size hi + size hd
            -- where 
                -- hi = Tree a
                -- hd = Tree a

-- 8, La función height, que dado un árbol retorna la altura del mismo
-- height :: Tree -> Int
-- height 

-- 9, Instanciar las clases Eq, Ord y Show para árboles binarios

--------------------------------------FIN PRÁCTICA 6------------------------------------------
-- Práctico 7
-- 1, Dada la definición del cuantificador N:
-- (Ni:Ri:Ti) = (Σi:Ri^Ti:1)
-- 1, Enunciar y demostrar la regla de partición de rango de la contatoria

-- <Ni:RvS:Ti> = <Ni:R:Ti> + <Ni:S:Ti>

-- <Ni:RvS:Ti>
-- {Definición de N}
-- <Σi:Ri^Ti:1>
-- <>

-- 2, Regla del rango vacío
-- <Ni:RvS:Ti> = <Ni:R:Ti> + <Ni:S:Ti>

-- 3,
-- (Σi : R.i ∧ T.i : K) = K ∗ (N i : R.i : T.i)
-- Partimos de una igualdad y llegamos a otra
 
-- K * (N i : R.i : T.i)
-- {deifinición de N}
-- K * (Σi : R.i ∧ T.i : 1) 
-- {distributiva}
-- (Σi : R.i ∧ T.i : k)

-- 2, definir la función nand a b = not sin utilizar not o &&
nand :: Bool -> Bool -> Bool
nand a b | a == False = True
         | b == False = True
         | otherwise = False

{- Otra forma
nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = False
-}

-- 3, Definir la función maj :: Bool -> Bool -> Bool -> Bool
maj :: Bool -> Bool -> Bool -> Bool
maj a b c | (a == False)&&(b == False) = False
          | (a == False)&&(c == False) = False
          | (b == False)&&(c == False) = False
          | otherwise = True

-- 4, En Haskell un predicado sobre un tipo A es una función p :: A -> Bool
-- ejemplo
-- even :: Int -> Bool
-- even x = x `mod` 2 == 0

-- a)
esPar :: Int -> Bool
esPar x = (mod x 2 == 0)

-- (∀i : 0 ≤ i < #xs : par xs.i)
todosPares :: [Int] -> Bool 
todosPares xs = and [esPar (xs !! i)|i <- [0..(length xs)-1]]
-- (Ei : 0 ≤ i < #xs : p xs.i)
algunPar :: [Int] -> Bool
algunPar xs = or [esPar (xs !! i) | i <- [0..(length xs)-1]]

-- Sin embargo, en Haskell es más fácil hacerlo así:
-- (∀i : 0 ≤ i < #xs : par xs.i)
todosPares' :: [Int] -> Bool 
todosPares' xs = and [esPar x | x <- xs]

-- (Ei : 0 ≤ i < #xs : p xs.i)
algunPar' :: [Int] -> Bool
algunPar' xs = or [esPar x | x <- xs]

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
        show Nil = "[]"
        show (Cons x xs) = "[" ++ mostrar (Cons x xs) ++ "]"
                    where mostrar (Cons y Nil) = show y
                          mostrar (Cons y ys) = show y ++ "," ++ mostrar ys

instance Eq a => Eq (List a) where
        (==) Nil Nil = True
        (==) Nil _ = False
        (==) _ Nil = False
        (==) (Cons x xs) (Cons y ys) = (x == y) && (xs == ys)

headl :: (List a) -> a
headl Nil = undefined
headl (Cons n _) = n

lastl :: (List a) -> a
lastl Nil = undefined
lastl (Cons n Nil) = n
lastl (Cons n m) = lastl m

concatl :: (List a) -> (List a) -> (List a)
concatl (Cons n m) Nil = (Cons n m)
concatl Nil (Cons a b) = (Cons a b)
concatl (Cons n m) (Cons a b) = (Cons n (concatl m (Cons a b)))

enRango :: Int -> Int -> [Int] -> [Int]
enRango a b xs = [x | x <- xs, x `elem` [a..b]]

andl :: Bool -> Bool -> Bool
andl False _ = False
andl _ False = False
andl _ _ = True

xorl :: Bool -> Bool -> Bool
xorl True False = True
xorl False True = True
xorl _ _ = False

imp :: Bool -> Bool -> Bool
imp p q | p == False = True
        | q == True = True
        | otherwise = False

inf :: Int
inf = inf +1

esPerm :: Ord a => [a] -> [a] -> Bool
esPerm [] _ = False
esPerm _ [] = False
esPerm xs ys | xs == ys = False
             | otherwise = (quick xs) == (quick ([a | a <- ys, a `elem` xs]))

esPerm2 :: Ord a => [a] -> [a] -> Bool
esPerm2 xs ys | length xs /= length ys = False
              | otherwise = quick xs == quick ys

data Tree a = Void | Nodo (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
        show Void = "<>"
        show (Nodo Void n Void) = "<" ++ show n ++ ">"
        show (Nodo hi n hd) = "<" ++ show hi ++ "," ++ show n ++ "," ++ show hd ++ ">"

size :: (Tree a) -> Int
size Void = 0
size (Nodo Void n Void) = 1
size (Nodo hi n hd) = 1 + size hi + size hd

instance Eq (Tree a) where
        (==) a b = (size a) == (size b)

prod :: [Int] -> Int
prod [] = 0
prod (x:xs) | xs == [] = x
            | otherwise = x * (prod xs)

esPrimo' :: Int -> Bool
esPrimo' n = and [(n `mod` i) /= 0 | i <- [2..(n-1)]]

prodPrim :: [Int] -> Int 
prodPrim xs = prod [xs!!i | i <- [0..((length xs)-1)], esPrimo (xs!!i)]

infi :: [Int]
infi = 1 : infi

plain :: [[a]] -> [a]
plain xs = foldr (++) [] xs

reversal :: [a] -> [a]
reversal [] = []
reversal (x:xs) = reversa xs ++ [x]