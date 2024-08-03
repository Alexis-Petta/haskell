{- Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista. P.ej. -}

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno numero  = any ((0==).mod numero)

{-Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento. P.ej.-}

promedio :: [[Float]]->[Float]
promedio = map promedioLista

promedioLista :: [Float] -> Float
promedioLista lista = sum lista/(fromIntegral.length) lista

{-Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 4 que no se cuenta -}

promediosSinAplazos :: [[Float]]->[Float]
promediosSinAplazos =  promedio.map filtrarMayoresASeis

filtrarMayoresASeis :: [Float] -> [Float]
filtrarMayoresASeis = filter (>= 6)

{-Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno-}

mejoresNotas :: [[Float]] -> [Float]
mejoresNotas = map maximum

{-Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más.-}

aprobo :: [Float]->Bool
aprobo notas = minimum notas >= 6

{-Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron.-}

aprobaron :: [[Float]]->[[Float]]
aprobaron = filter aprobo

{-Definir la función divisores/1, que recibe un número y devuelve la lista de divisores. -}

divisores :: Int -> [Int]
divisores n = filter (esDivisor n) [1..n]

esDivisor :: Int->Int->Bool
esDivisor n = (==0).mod n