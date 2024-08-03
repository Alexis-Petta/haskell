import Data.ByteString.Builder (FloatFormat)
import Distribution.Simple.Command (OptDescr(BoolOpt))
--Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero
esMultiplo :: Int -> Int -> Bool
esMultiplo multiplo numero = mod numero multiplo == 0

--Definir la función cubo/1, devuelve el cubo de un número.
cubo :: Int -> Int
cubo numero = numero*numero*numero

--Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura.
area :: Int -> Int -> Int
area base altura = base*altura

--Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) Nota: Resolverlo reutilizando la función esMultiploDe/2
esBisiesto :: Int -> Bool
esBisiesto anio = esMultiplo 400 anio || (esMultiplo 4 anio && not (esMultiplo 100 anio))

--Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit.
celsiusToFahr:: Float -> Float
celsiusToFahr grados = grados*1.8+32

--Definir la función fahrToCelsius/1, la inversa de la anterior.
fahrToCelsius:: Float -> Float
fahrToCelsius grados = (grados-32)/1.8

--Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 
haceFrio:: Float -> Bool
haceFrio grados = fahrToCelsius grados < 8

--Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula:  m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 
mcm:: Int -> Int -> Int
mcm a b = (a*b) `div` mcd a b

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

--Dispersión
--Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos; cada medición es un entero que representa una cantidad de cm. 
--P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
--A partir de estos tres números, podemos obtener algunas conclusiones. 

--dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos.
--De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min, que estamos usando). 

--diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, que son días locos si la dispersión es grande,
--y que son días normales si no son ni parejos ni locos. Una dispersión se considera chica si es de menos de 30 cm, y grande si es de más de un metro. 
--Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas. 

dispersion:: Float -> Float -> Float -> Float
dispersion valor1 valor2 valor3 = elMaximo valor1 valor2 valor3 - elMinimo valor1 valor2 valor3

elMaximo :: Float -> Float -> Float -> Float
elMaximo valor1 valor2 valor3
    | valor1 >= valor2 && valor1 >= valor3 = valor1
    | valor2 >= valor1 && valor2 >= valor3 = valor2
    | otherwise = valor3

elMinimo :: Float -> Float -> Float -> Float
elMinimo valor1 valor2 valor3
    | valor1 <= valor2 && valor1 <= valor3 = valor1
    | valor2 <= valor1 && valor2 <= valor3 = valor2
    | otherwise = valor3

diasParejos :: Float -> Float -> Float -> Bool
diasParejos valor1 valor2 valor3 = dispersion valor1 valor2 valor3 <30

diasLocos :: Float -> Float -> Float -> Bool
diasLocos valor1 valor2 valor3 = dispersion valor1 valor2 valor3 >100

diasNormales :: Float -> Float -> Float -> Bool
diasNormales valor1 valor2 valor3 = not (diasParejos valor1 valor2 valor3) && not (diasLocos valor1 valor2 valor3)


--En una plantación de pinos, de cada árbol se conoce la altura expresada en cm. El peso de un pino se puede calcular a partir de la altura así:
-- 3 kg x cm hasta 3 metros, 2 kg x cm arriba de los 3 metros. P.ej. 2 metros ⇒  600 kg, 5 metros ⇒  1300 kg. 
--Los pinos se usan para llevarlos a una fábrica de muebles, a la que le sirven árboles de entre 400 y 1000 kilos, un pino fuera de este rango no le sirve a la fábrica. Para esta situación: 
--Definir la función pesoPino, recibe la altura de un pino y devuelve su peso. 
--Definir la función esPesoUtil, recibe un peso en kg y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. 
--Definir la función sirvePino, recibe la altura de un pino y devuelve True si un pino de ese peso le sirve a la fábrica, y False en caso contrario. Usar composición en la definición. 

pesoPino :: Int -> Int
pesoPino altura 
 | altura<3000 = altura * 3
 | otherwise = altura * 2 

esPesoUtil :: Int -> Bool
esPesoUtil peso = peso>400 && peso<1000

sirvePino :: Int -> Bool
sirvePino = esPesoUtil . pesoPino

--Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar la función esCuadradoPerfecto/1, sin hacer operaciones con punto flotante. 
--Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. 
--Pensar que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. 
--También algo de recursividad van a tener que usar. 

esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto n = esCuadradoPerfectoAux n 0 0

esCuadradoPerfectoAux :: Int -> Int -> Int -> Bool
esCuadradoPerfectoAux n suma cuadrado
    | cuadrado == n = True
    | cuadrado > n = False
    | otherwise = esCuadradoPerfectoAux n (suma + 2) (cuadrado + suma + 1)
