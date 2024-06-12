module Lib where
import PdePreludat

data Planeta = Planeta {
    nombrePlaneta :: String,
    posicion:: Posicion,
    tiempo :: Number -> Number
}

data Astronauta = Astronauta {
    nombreAstronauta :: String,
    edad :: Number,
    planeta :: Planeta
}

type Posicion = (Number, Number, Number)

coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

--punto 1
--a)
distancia:: Planeta -> Planeta -> Number
distancia p1 p2 = 10
 
--b)
tiempoEntrePlanetas :: Planeta -> Planeta -> Number -> Number
tiempoEntrePlanetas p1 p2 velocidad = ((/velocidad).distancia p1) p2

--punto 2
pasarTiempo :: Astronauta -> Number -> Astronauta
pasarTiempo astronauta =  envejecer astronauta . tiempo (planeta astronauta)

envejecer :: Astronauta -> Number -> Astronauta
envejecer astronauta anios = astronauta {edad = edad astronauta + anios}

--punto 3
type Nave = Planeta -> Planeta -> Number

naveVieja :: Number -> Nave
naveVieja tanques origen destino
    |tanques < 6 = tiempoEntrePlanetas origen destino 10
    |otherwise = tiempoEntrePlanetas origen destino 10

naveFuturista :: Nave
naveFuturista _ _ = 0

realizarViaje :: Nave -> Planeta -> Planeta -> Astronauta -> Astronauta
realizarViaje nave origen destino astronauta = (cambiarPlaneta destino.pasarTiempo astronauta) (nave origen destino)

cambiarPlaneta :: Planeta -> Astronauta -> Astronauta
cambiarPlaneta nuevoPlaneta astronauta = astronauta {planeta = nuevoPlaneta}
--punto 4
rescate :: [Astronauta] -> Astronauta -> Planeta -> Planeta -> Nave -> [Astronauta]
rescate rescatistas varado planetaRescatistas planetaVarado nave = cambiarEdadTodos (rescatistas ++ pasarTiempo varado (nave origen destino)) origen destino nave

cambiarEdadTodos :: [Astronauta] -> Planeta -> Planeta -> Nave -> [Astronauta]
cambiarEdadTodos astronautas origen destino nave = map (realizarViaje nave origen destino) astronautas