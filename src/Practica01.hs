module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle rad) = pi * rad^2
area (Square lado) = lado^2
area (Rectangle b h) = b*h
area (Triangle lad) = ((lad^2)*(sqrt 3))/4
area (Trapeze bMay bMen h) = ((bMay + bMen)*h)/2

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle rad) = 2*pi*rad
perimeter (Square lado) = lado*lado
perimeter (Rectangle b h) = 2*b + 2*h
perimeter (Triangle lad) = 3*lad
perimeter (Trapeze bMay bMen h) =
--Funcion auxiliar para calcular un lado del trapecio
    let ladoT = sqrt(h^2+(bMay - bMen)^2)
    in bMay + bMen + 2*ladoT


--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt (((x2 - x1)^2) + ((y2 - y1)^2))

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x, y) = distance (0,0) (x, y)

--Ejercicio 3
data Haskellium = Haskellium { --el constructor "Haskellium" recibirá 5 datos
            name :: String,
            lastName1 :: String,
            lastName2 :: String,
            location :: Point,
            houseShape :: Shape
} deriving (Show) --para que se imprima en la consola

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son p1 p2 sonName =   
    Haskellium
        sonName
        (lastName1 p1)
        (lastName1 p2)
        (location p1)
        (houseShape p1)   --se tomara como su hogar y locación la del primer padre

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float  -- sumaremos las dos funciones anteriormente creadas
houseCost person =
    (perimeter figura * 2.5) + area figura -- el área de las paredes es el perimetro por la altura, más la área del techo que es el área de la figura
    where figura = houseShape person  -- donde se encunetre la figura será el hogar del nuevo Haskellium c:


--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork person    --segun la formua del tiempo = distancia/velocidad         
    | distance < 300 = distance / 30  --si la distancia es menor a 300 (bici)
    | otherwise  = distance / 70 --en otro caso (moto)
    where distance = from0 (location person) --al definir la distancia sabremos las unidades de tiempo que Hakellium necesita para llegar el punto de origen = lugar de trabajo


--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
--usando funcion reverse para voltear la cadena
palindromo xs = xs == reverse xs

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
-- caso base lista vacia
myFoldr f acumulador [] = acumulador    --f es la funcion que recibe
--caso recursivo, se evalua primero la cola y despues se aplica f a la cabeza
myFoldr f acumulador (x:xs) = f x (myFoldr f acumulador xs)

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined

--ARBOLES

--Implementacion

data OneTwoTree a = Void  --nodo vacio
                  | Node a (OneTwoTree a)  --nodo con un elemento y un hijo
                  | Branch a (OneTwoTree a) (OneTwoTree a)  --rama con un elemento y dos hijos
                   deriving (Show)  --mostrar en terminal

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Void = 0  --no hay ningun valor
suma (Node a b) = a + suma b  --tomar el valor del nodo a y sumarlo al del nodo b
suma (Branch a b1 b2) = a + suma b1 + suma b2  --tomar el valor del nodo com dos hijos y sumarlo al valor de cada uno de los hijos del nodo
