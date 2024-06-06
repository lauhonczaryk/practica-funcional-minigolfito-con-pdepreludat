module Lib where
import PdePreludat

-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun:: Ord a1 => (a2->a1)->[a2]->a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

----------------------------------------------
---- Resolución del ejercicio
----------------------------------------------
--PUNTO 1
type Palo = Habilidad -> Tiro

putter:: Palo
putter habilidad = UnTiro 10 (precisionJugador habilidad * 2) 0

madera:: Palo
madera habilidad = UnTiro 100 (precisionJugador habilidad / 2) 5

hierros :: Number -> Palo
hierros n habilidad = UnTiro (fuerzaJugador habilidad*n) (precisionJugador habilidad / n) (max 0 (n-3))

palos:: [Palo]
palos = [putter, madera] ++ map hierros [1..10]

--PUNTO 2
golpe:: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

--PUNTO 3
data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
}
superaTunelRampita :: Tiro -> Bool
superaTunelRampita tiro =  precision tiro > 90 && altura tiro == 0
efectoTunelRampita :: Tiro -> Tiro
efectoTunelRampita tiro = UnTiro (velocidad tiro *2) 120 0

tunelRampita ::Obstaculo
tunelRampita = UnObstaculo superaTunelRampita efectoTunelRampita 

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && altura tiro >=1 && altura tiro <=5
efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largoLaguna tiro = UnTiro (velocidad tiro) (precision tiro) (altura tiro/largoLaguna)

laguna :: Number->Obstaculo
laguna largoLaguna = UnObstaculo superaLaguna (efectoLaguna largoLaguna)

superaHoyo :: Tiro -> Bool
superaHoyo tiro =  velocidad tiro >= 5 && velocidad tiro <= 20 && altura tiro == 0 && precision tiro> 95
efectoHoyo :: Tiro -> Tiro
efectoHoyo tiro = UnTiro 0 0 0

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo


--PUNTO 4
superaObsPalo :: Jugador -> Obstaculo -> Palo -> Bool
superaObsPalo jugador obs palo = puedeSuperar obs (golpe jugador palo) 

palosUtiles:: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObsPalo jugador obstaculo) palos

superaObsTiro :: Tiro -> Obstaculo -> Bool
superaObsTiro tiro obs = puedeSuperar obs tiro

cuantosObsSupera :: Tiro -> [Obstaculo] -> Number
cuantosObsSupera tiro obss = length (takeWhile (superaObsTiro tiro) obss)

--cuantos obstaculos supera ese palo
cuantoObsSuperaPalo:: Jugador-> [Obstaculo] ->Palo ->Number
cuantoObsSuperaPalo jug obss palo= cuantosObsSupera (golpe jug palo) obss

paloMasUtil:: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obss = maximoSegun (cuantoObsSuperaPalo jugador obss) palos 

--PUNTO 5
{-
Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos
 ganó cada niño al finalizar el torneo, se pide retornar la lista de padres que 
 pierden la apuesta por ser el “padre del niño que no ganó”. 
Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.
-}
{-
maximoSegun:: Ord b => (a->b)->[a]->a
maximoSegun f = foldl1 (mayorSegun f)
a:   b:-}
type Resultado = (Jugador,Puntos)



-- armar lista de todos menos ganador
