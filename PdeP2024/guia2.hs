--Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 
siguiente :: Int->Int
siguiente = (1+)

--Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número, ej
mitad :: Float -> Float
mitad = (0.5 *)

--Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa
inversa :: Float -> Float
inversa = ((-1)/)

--Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.
triple :: Float -> Float
triple = (3*)

--Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario
esNumeroPositivo :: Int -> Bool
esNumeroPositivo = (0<)

--Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición.
esMultiplo :: Int -> Int -> Bool
esMultiplo multiplo = (==0).(`mod` multiplo)

--Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.
esBisiesto :: Int -> Bool
esBisiesto anio = (esMultiplo 400 anio ||) . (esMultiplo 4 anio &&) . not . esMultiplo 100 $ anio

--Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada. 
--Nota: Resolverlo utilizando la función inversa Ej. 2.3, sqrt y composición.
inversaRaizCuadrada :: Float -> Float
inversaRaizCuadrada = inversa . sqrt

--Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n por Ej: 
--Incrementa 2 al cuadrado de 3, da como resultado 11. Nota: Resolverlo utilizando aplicación parcial y composición. 
incrementMCuadradoN :: Float -> Float -> Float
incrementMCuadradoN m= (+m) . (^2)

--Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par. 
esResultadoPar :: Int -> Int -> Bool
esResultadoPar n = even . (^n)