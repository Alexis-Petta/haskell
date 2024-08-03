{-
1) Modelar a los héroes. Tip: lean todo el enunciado!
-}

data Heroe = Heroe {
    reconocimiento :: Int,
    epiteto :: String,
    artefactos :: [Artefacto],
    tareas :: [String]
} deriving (Show)

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
} deriving (Show)


heroeEjemplo :: Heroe
heroeEjemplo = Heroe {
    reconocimiento = 600,
    epiteto = "El valiente",
    artefactos = [Artefacto "Escudo de Troya" 50, Artefacto "Casco de Ares" 75],
    tareas = ["Rescatar a la princesa", "Defender la ciudad"]
}

{-
Hacer que un héroe pase a la historia. Esto varía según el índice de reconocimiento que tenga el
héroe a la hora de su muerte:
a) Si su reconocimiento es mayor a 1000, su epíteto pasa a ser "El mítico", y no obtiene ningún
artefacto. ¿Qué artefacto podría desear tal espécimen?
b) Si tiene un reconocimiento de al menos 500, su epíteto pasa a ser "El magnífico" y añade a sus
artefactos la lanza del Olimpo (100 de rareza).
c) Si tiene menos de 500, pero más de 100, su epíteto pasa a ser "Hoplita" y añade a sus
artefactos una Xiphos (50 de rareza).
d) En cualquier otro caso, no pasa a la historia, es decir, no gana ningún epíteto o artefacto
-}

pasarHistoria :: Heroe -> Heroe
pasarHistoria heroe
 |reconocimiento heroe > 1000 = Heroe (reconocimiento heroe) "El mitico" [] (tareas heroe)
 |reconocimiento heroe > 500  = Heroe (reconocimiento heroe) "El magnifico" (artefactos heroe ++ [Artefacto "La lanza del olimpo" 100]) (tareas heroe)
 |reconocimiento heroe > 100  = Heroe (reconocimiento heroe) "Hoplita" (artefactos heroe ++ [Artefacto "Xiphos" 50]) (tareas heroe)
 |otherwise                   = heroe

{-Día a día, los héroes realizan tareas. Llamamos tareas a algo que modifica a un héroe de alguna manera,
algo tan variado como aumentar su reconocimiento, obtener un nuevo epíteto o artefacto, y muchas más. Tras
realizar una tarea, los héroes se la anotan en una lista, para luego recordarlas y presumirlas ante sus
compañeros. Hay infinidad de tareas que un héroe puede realizar, por el momento conocemos las siguientes:

● Encontrar un artefacto: el héroe gana tanto reconocimiento como rareza del artefacto, además de
guardarlo entre los que lleva.

● Escalar el Olimpo: esta ardua tarea recompensa a quien la realice otorgándole 500 unidades de
reconocimiento y triplica la rareza de todos sus artefactos, pero desecha todos aquellos que luego de
triplicar su rareza no tengan un mínimo de 1000 unidades. Además, obtiene "El relámpago de Zeus"
(un artefacto de 500 unidades de rareza).

● Ayudar a cruzar la calle: incluso en la antigua Grecia los adultos mayores necesitan ayuda para ello.
Los héroes que realicen esta tarea obtiene el epíteto "Groso", donde la última 'o' se repite tantas veces
1 Epíteto: apodo por el cual puede llamarse a alguien en lugar de su nombre y sigue identificando al sujeto.
como cuadras haya ayudado a cruzar. Por ejemplo, ayudar a cruzar una cuadra es simplemente
"Groso", pero ayudar a cruzar 5 cuadras es "Grosooooo".

● Matar una bestia: Cada bestia tiene una debilidad (por ejemplo: que el héroe tenga cierto artefacto, o
que su reconocimiento sea al menos de tanto). Si el héroe puede aprovechar esta debilidad, entonces
obtiene el epíteto de "El asesino de <la bestia>". Caso contrario, huye despavorido, perdiendo su
primer artefacto. Además, tal cobardía es recompensada con el epíteto "El cobarde".
3) Modelar las tareas descritas, contemplando que en el futuro podría haber más.
-}

realizarTarea :: Heroe -> (a->b) -> Heroe
realizarTarea heroe tarea = undefined

encontrarArtefacto :: Heroe -> Artefacto -> Heroe
encontrarArtefacto heroe artefacto = heroe {reconocimiento=reconocimiento heroe + rareza artefacto, artefactos=artefacto:artefactos heroe}

-- Función para triplicar la rareza de un artefacto
triplicarRareza :: Artefacto -> Artefacto
triplicarRareza (Artefacto nombre rareza) = Artefacto nombre (rareza * 3)

-- Función para escalar el Olimpo
escalarOlimpo :: Heroe -> Heroe
escalarOlimpo heroe = heroe {
    reconocimiento = reconocimiento heroe + 500,
    artefactos = agregarRelampagoDeZeus.filtrarArtefactos.triplicarRarezas.artefactos $ heroe
}

-- Función para triplicar la rareza de todos los artefactos
triplicarRarezas :: [Artefacto] -> [Artefacto]
triplicarRarezas = map triplicarRareza

-- Función para filtrar artefactos con rareza mayor o igual a 1000
filtrarArtefactos :: [Artefacto] -> [Artefacto]
filtrarArtefactos = filter (\artefacto -> rareza artefacto >= 1000)

-- Función para agregar "El relámpago de Zeus"
agregarRelampagoDeZeus :: [Artefacto] -> [Artefacto]
agregarRelampagoDeZeus artefactos = Artefacto "El relámpago de Zeus" 500 : artefactos

cruzarCalle :: Heroe -> Int -> Heroe
cruzarCalle heroe numeroCalles = heroe {epiteto= "gros" ++ replicate numeroCalles 'o'}

