-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


-- PUNTO 1 -- 

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = 
    UnTiro 
    {
        velocidad = 10, 
        precision = precisionJugador habilidad*2, 
        altura = 0
    }

madera :: Palo
madera habilidad = 
    UnTiro 
    {
        velocidad = 100, 
        precision = precisionJugador habilidad `div` 2,
        altura = 5
    }

hierro :: Int -> Palo
hierro n habilidad = 
    UnTiro 
    {
        velocidad = ((n*).fuerzaJugador $ habilidad), 
        precision = precisionJugador habilidad `div` n , 
        altura = n-3 `max` 0
    }

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- Punto 2 --   

golpe ::  Palo -> Jugador -> Tiro
golpe palo = palo.habilidad

-- Punto 3 -- 

type Obstaculo = Tiro -> Bool 
superaObstaculo :: Obstaculo -> (Tiro -> Tiro) -> Tiro -> Tiro
superaObstaculo obstaculo efecto tiro
  | obstaculo tiro = efecto tiro
  | otherwise = UnTiro 0 0 0

tunel :: Obstaculo
tunel = (90 <) . precision
efectoTunel :: Tiro -> Tiro
efectoTunel tiro = UnTiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}

laguna :: Obstaculo
laguna tiro = ((80<) . velocidad) tiro && (between 1 5 . altura) tiro
efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna grosor tiro = UnTiro {velocidad = velocidad tiro, precision = precision tiro, altura = altura tiro `div` grosor}

hoyo :: Obstaculo
hoyo tiro = (between 5 20 . velocidad) tiro && ((0==) . altura) tiro && ((95<) . precision) tiro
efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = UnTiro 0 0 0



{-Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene, quedando con todos sus componentes en 0.


Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.
Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo,
el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.
Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más obstáculos con un solo tiro.
-}

data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
  }


palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo palo = puedeSuperar obstaculo (golpe jugador palo)