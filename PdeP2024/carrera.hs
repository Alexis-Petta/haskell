data Auto = Auto{
    color :: String,
    velocidad :: Float,
    distancia :: Float
}deriving (Show, Eq)

type Carrera = [Auto]

-- PUNTO 1 --
estaCerca :: Auto -> Auto -> Bool
estaCerca primerAuto segundoAuto = primerAuto /= segundoAuto && distanciaEntre primerAuto segundoAuto < 10

distanciaEntre :: Auto -> Auto -> Float
distanciaEntre primerAuto = abs . (distancia primerAuto -) . distancia

vaTranquilo ::Auto -> Carrera -> Bool
vaTranquilo auto carrera = (not.algunAutoCerca auto) carrera && vaGanando auto carrera

algunAutoCerca :: Auto -> Carrera -> Bool
algunAutoCerca auto = any (estaCerca auto) 

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto = all (leVaGanando auto) . filter (/= auto)

leVaGanando :: Auto -> Auto -> Bool
leVaGanando auto1 = (< distancia auto1). distancia

posicionAuto :: Auto -> Carrera -> Int
posicionAuto auto  = (1+) . length . filter (not . leVaGanando auto) 

-- PUNTO 2 --
correr ::Float -> Auto ->  Auto
correr  tiempo auto = auto {distancia = distancia auto + velocidad auto * tiempo}

type ModificadorDeVelocidad = Float -> Float
alterarVelocidad :: ModificadorDeVelocidad -> Auto -> Auto
alterarVelocidad modificador auto = auto {velocidad= (modificador.velocidad) auto}

bajarVelocidad :: Float -> Auto -> Auto
bajarVelocidad reduccion = alterarVelocidad (reduccion-)

-- PUNTO 3 --
{-Como se explicó inicialmente sobre las carreras que queremos simular, los autos que participan pueden gatillar poderes especiales a los que denominamos power ups.
Estos poderes son variados y tienen como objetivo impactar al estado general de la carrera, ya sea afectando al auto que lo gatilló y/o a sus contrincantes dependiendo de qué poder se trate.

Nota: disponemos de una función afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a] que puede ser de utilidad para manipular el estado de la carrera. Ver pág. 2 para más detalles.

Inicialmente queremos poder representar los siguientes power ups, pero debería ser fácil incorporar más power ups a futuro para enriquecer nuestro programa:
terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.
miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.
jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.

Como se mencionó anteriormente, disponemos de la siguiente función para usar dentro de la resolución:

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista
-}

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> Carrera -> Carrera
terremoto :: PowerUp
terremoto autoQueGatillo =
    afectarALosQueCumplen (estaCerca autoQueGatillo) (bajarVelocidad 50)

miguelitos :: Float -> PowerUp
miguelitos velococidadAReducir autoQueGatillo  = 
    afectarALosQueCumplen (leVaGanando autoQueGatillo) (bajarVelocidad velococidadAReducir) 

jetPack :: Float -> PowerUp
jetPack tiempo autoQueGatillo =
    afectarALosQueCumplen (== autoQueGatillo) (alterarVelocidad (\ _ -> velocidad autoQueGatillo) . correr tiempo . alterarVelocidad (*2))