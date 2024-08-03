-------------------------------------DATAS------------------------------------------------
data Auto = Auto {
 color :: String,
 velocidad :: Int,
 distancia :: Int
} deriving (Show, Eq)

data Carrera = Carrera{
    posicion :: Int,
    auto :: Auto
}
-------------------------------------PARA EJEMPLOS----------------------------------------
autito :: Auto
autito = Auto "ale" 150 35

autos :: [Auto]
autos =
  [ Auto "rojo" 120 50
  , Auto "verde" 100 30
  , Auto "azul" 110 45
  , Auto "amarillo" 90 25
  ]

carreraInicial :: Carrera
carreraInicial = Carrera 0 autito
-------------------------------------CODIGO------------------------------------------------

--PUNTO 1--
estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = color auto1 /= color auto2 && abs (distancia auto1 - distancia auto2) <= 10

vaTranquilo :: Auto -> [Auto] -> Bool
vaTranquilo auto1 (a:as)
 |null as= True
 |not (estaCerca auto1 a) = vaTranquilo auto1 as
 |estaCerca auto1 a = False

puesto :: Carrera -> [Auto] -> Int
puesto carrera (a:as)
 |null as = posicion carrera + 1
 |distancia a>distancia (auto carrera) = puesto (Carrera (posicion carrera+1) (auto carrera)) as
 |otherwise = puesto (Carrera (posicion carrera) (auto carrera)) as

 --PUNTO 2--
correr :: Auto -> Int -> Auto
correr auto tiempo = Auto (color auto) (velocidad auto) (distancia auto+tiempo*velocidad auto)

modificador :: (Int->Int->Int)->Auto->Int->Int
modificador f auto = f (velocidad auto)

--modificador (-) autito (velocidad autito)

--PUNTO 3--

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista
--afectarALosQueCumplen (estaCerca autito) terremoto autos 
--terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.
terremoto :: Auto->Auto
terremoto auto  = Auto (color auto) (modificador (-) auto 50) (distancia auto)
--miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso.
-- Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.
miguelitos :: Auto->Int->Auto
miguelitos auto velocidadARestar = Auto (color auto) (modificador (-) auto velocidadARestar) (distancia auto)
{-jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, 
el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente su velocidad vuelve al valor que 
tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.-}
jetpack :: Auto->Int->Auto
jetpack auto tiempo = miguelitos (correr (Auto (color auto)(2*velocidad auto)(distancia auto)) tiempo) (velocidad auto) 

