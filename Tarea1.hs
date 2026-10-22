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
leerHora = undefined

duracion :: String -> String -> Either String Int
duracion = undefined

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
