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
data Haskellium = Undefined

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son = undefined

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost = undefined

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork = undefined

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo = undefined

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr = undefined

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia = undefined

--ARBOLES

--Implementacion

data OneTwoTree a = Void
                  | Node a (OneTwoTree a)
                  | Branch a (OneTwoTree a) (OneTwoTree a)
                   deriving (Show)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma Void = 0
suma (Node a b) = a + suma b
suma (Branch a b1 b2) = a + suma b1 + suma b2