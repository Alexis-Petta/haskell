{-Ejercicio: Filtrado de Números Pares

Define una función en Haskell llamada filtrarParesMenoresA que tome dos argumentos:

Una lista de enteros.
Un entero n.
La función debe devolver una nueva lista que contenga todos los números pares de la lista original que sean menores que n. Utiliza composición parcial para lograr esto.-}

filtrarParesMenoresA :: [Int]->Int->[Int]
filtrarParesMenoresA lista n = filter (n<).filter even $ lista

{-Ejercicio: Suma de Elementos Mayores a un Valor

Define una función en Haskell llamada sumaMayoresA que tome dos argumentos:

Una lista de números enteros.
Un número entero m.
La función debe devolver la suma de todos los números en la lista que sean mayores que m.-}

sumaMayoresA :: [Int] -> Int -> Int
sumaMayoresA lista n = sum.filter (>n) $ lista


promedioMenoresOIgualesA :: [Int] -> Int -> Int
promedioMenoresOIgualesA lista k = (flip div ((length.filter (k>=)) lista).sum.filter (k>=)) lista

