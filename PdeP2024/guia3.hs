{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}
import Language.Haskell.TH (tupleDataName)
-- Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el elemento correspondiente, p.ej.
fst3 :: (a, b, c) -> a
fst3 (primer, segundo, tercer)= primer

snd3 :: (a, b, c) -> b
snd3 (primer, segundo, tercer)= segundo

trd3 :: (a, b, c) -> c
trd3 (primer, segundo, tercer)= tercer


--Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, me devuelve como resultado una tupla con el resultado de aplicar 
--el elemento a cada una de la funciones, ej: 

aplicar :: (Int -> a, Int -> b) -> Int -> (a, b)
aplicar (primer, segundo) numero = (primer numero, segundo numero)

--Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a llevarle 10, devuelve el producto. Ej: 
cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (a, b)
    | a > b = a + b
    | b - a > 10 = b - a
    | otherwise = a * b

--Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), p.ej. un patito en el 1ro y un 7 en el 2do se representan mediante el par (2,7). 
--A partir de esto: 
--Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas. 
--Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo. 
--Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 15 y además haberse sacado al menos 7 en cada parcial. 
--Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición. La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 

esNotaBochazo :: Int -> Bool
esNotaBochazo = (5<)

aprobo :: (Int, Int) -> Bool
aprobo (a, b)= not (esNotaBochazo a && esNotaBochazo b)

promociono :: (Int, Int) -> Bool
promociono (a, b) = a>6 && b>6 && a+b>14

consulta :: (Int, Int) -> Bool
consulta = esNotaBochazo.fst

--Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos mediante un par de pares ((parc1,parc2),(recup1,recup2)). 
--Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. 
--Observamos que con la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. 
--Considerar que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior. 
--Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales del alumno para el 1er y el 2do parcial. P.ej. 

notasFinales :: ((Int, Int), (Int, Int)) -> (Int, Int)
notasFinales ((parcial1, parcial2), (recu1, recu2)) = (mayor parcial1 recu1, mayor parcial2 recu2)

mayor :: Int -> Int -> Int
mayor a b
 |a>b=a
 |otherwise=b

--Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recursa o no. O sea, la respuesta debe ser True si recursa, y False si no recursa.
--Usar las funciones definidas en este punto y el anterior, y composición. La consulta debe tener esta forma:
ejemplo :: ((Int, Int), (Int, Int))
ejemplo = ((2,7),(6,-1))
recursa :: ((Int, Int), (Int, Int)) -> Bool
recursa = aprobo . notasFinales


--Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recuperó el primer parcial. Usar composición. La consulta debe tener esta forma:
recupero:: ((Int, Int), (Int, Int)) -> Bool
recupero = (/= (-1)) . fst.snd

--Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve True si el alumno, pudiendo promocionar con los parciales (o sea sin recup.), 
--igual rindió al menos un recup. Vale definir funciones auxiliares. Hacer una definición que no use pattern matching, en las eventuales funciones auxiliares tampoco; o sea, 
--manejarse siempre con fst y snd.
recuperoDeGusto :: ((Int, Int), (Int, Int)) -> Bool
recuperoDeGusto notas = recupero notas && (promociono.fst) notas


--Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva True si es mayor de 21 años y False en caso contrario. Por Ej:.
--Nota: Definir la función utilizando aplicación parcial y composición.

esMayorDeEdad :: (String, Int) -> Bool
esMayorDeEdad = (20<).snd

--Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo duplica, sino lo deja como está y con el segundo elemento en caso de ser impar le suma 1 
--y si no deja esté último como esta. 
calcular :: (Int, Int) -> (Int,Int)
calcular = doble.sumar1

doble :: (Int, Int) -> (Int, Int)
doble tupla
 |(even.fst) tupla = (((2*).fst) tupla, snd tupla)
 |otherwise = tupla

sumar1 :: (Int, Int) -> (Int, Int)
sumar1 tupla
 |(not.even.snd) tupla = (fst tupla, ((1+).snd) tupla)