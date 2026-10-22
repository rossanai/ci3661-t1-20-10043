-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Rossana Ilardo
--  Carnet: 20-10043
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
-- Quita lo que no sea letra o dígito y pasa todo a minúscula
limpiar :: String -> String
limpiar [] = []
limpiar (c:cs)
  | isAlphaNum c = toLower c : limpiar cs
  | otherwise    = limpiar cs

  -- Le da la vuelta a una lista
invertir :: [a] -> [a]
invertir [] = []
invertir (x:xs) = invertir xs ++ [x]

esPalindromo :: String -> Bool
esPalindromo texto = comparar (limpiar texto)

-- Indica si una cadena es igual a su inversa
comparar :: String -> Bool
comparar s = s == invertir s

-- A2
-- Va avanzando por la lista y a la vez lleva anotados el mayor y el menor que ve. Cada uno se compara una vez
-- con esos dos valores y luego se pasa al siguiente

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (recorre x x xs)

-- Lleva el mayor y el menor vistos hasta ahora mientras avanza por la lista
recorre :: Ord a => a -> a -> [a] -> (a, a)
recorre mayor menor [] = (mayor, menor)
recorre mayor menor (y:ys) = recorre (elmayor mayor y) (elmenor menor y) ys

-- De dos valores, devuelve el más grande
elmayor :: Ord a => a -> a -> a
elmayor a b
  | b > a     = b
  | otherwise = a

-- De dos valores, devuelve el más pequeño
elmenor :: Ord a => a -> a -> a
elmenor a b
  | b < a     = b
  | otherwise = a

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [a, b, ':', c, d]
  | todosdigitos a b c d = validar (juntar a b) (juntar c d)
  | otherwise            = Left "formato inválido"
leerHora _ = Left "formato inválido"

-- Indica si los cuatro son dígitos
todosdigitos :: Char -> Char -> Char -> Char -> Bool
todosdigitos a b c d = isDigit a && isDigit b && isDigit c && isDigit d

-- Convierte dos dígitos en un número
juntar :: Char -> Char -> Int
juntar x y = 10 * digitToInt x + digitToInt y

-- Revisa los rangos, hora y minutos
validar :: Int -> Int -> Either String (Int, Int)
validar h m
  | h > 23    = Left "hora fuera de rango"
  | m > 59    = Left "minutos fuera de rango"
  | otherwise = Right (h, m)

duracion :: String -> String -> Either String Int
duracion s1 s2 = case leerHora s1 of
  Left e -> Left e
  Right (h1, m1) -> case leerHora s2 of
    Left e -> Left e
    Right (h2, m2) -> Right (minutosEntre (h1 * 60 + m1) (h2 * 60 + m2))

-- Minutos entre dos momentos del día, pasando a mañana si el final es anterior
minutosEntre :: Int -> Int -> Int
minutosEntre ini fin
  | fin >= ini = fin - ini
  | otherwise  = fin - ini + 1440

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral umbral xs = resultado (foldl paso (0, 0) xs)
  where
    -- El acumulador es (suma, cuenta) solo cambia si el elemento es estrictamente mayor que el umbral
    paso (suma, cuenta) x
      | x > umbral = (suma + x, cuenta + 1)
      | otherwise  = (suma, cuenta)
    -- Si no se contó ningún elemento no hay promedio, si no, suma / cuenta
    resultado (suma, cuenta)
      | cuenta == 0 = Nothing
      | otherwise   = Just (suma / cuenta)

-- B2
-- Los desplazamientos de un caballo son df, dc con |df|=/=|dc|y los dos en [-2, -1, 1, 2]. Como df se recorre de menor a mayor 
-- y dentro de cada df, dc también, las casillas ya salen ordenadas

movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo (f, c) =
  [ (f + df, c + dc) | df <- [-2, -1, 1, 2], dc <- [-2, -1, 1, 2]
                     , abs df /= abs dc
                     , dentro (f + df), dentro (c + dc) ]

-- Indica si una coordenada está entre 1 y 8
dentro :: Int -> Bool
dentro n = n >= 1 && n <= 8

-- Casillas alcanzables en dos saltos, sin repetidos
enDosSaltos :: (Int, Int) -> Int
enDosSaltos p = length (foldr agregar [] dosSaltos)
  where
    dosSaltos = [ r | q <- movimientosCaballo p, r <- movimientosCaballo q ]
    agregar x acc = if x `elem` acc then acc else x : acc

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
