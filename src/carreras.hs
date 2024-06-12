module Lib where
import PdePreludat

data Auto = Auto {
    color :: String,
    velocidad :: Number,
    distanciaRecorrida :: Number
} deriving (Show, Eq)

type Carrera = [Auto]

--PUNTO 1
--a)
estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaEntreAutos auto1 auto2 < 10

distanciaEntreAutos :: Auto -> Auto -> Number
distanciaEntreAutos auto1 auto2 = abs(distanciaRecorrida auto1 - distanciaRecorrida auto2)

--b)
vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto carrera = (not . tieneAutoCerca auto) carrera && vaGanando auto carrera

tieneAutoCerca:: Auto -> Carrera -> Bool
tieneAutoCerca auto = any (estaCerca auto)

vaGanando'::  Auto -> Carrera -> Bool
vaGanando' auto carrera = distanciaRecorrida auto == maximum (map distanciaRecorrida carrera)

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto = all (leVaGanando auto) . filter (/= auto)

leVaGanando :: Auto -> Auto -> Bool
leVaGanando ganador = (< distanciaRecorrida ganador).distanciaRecorrida

--c)
puesto :: Auto -> Carrera -> Number
puesto auto = (1 +) .length.filter (not.leVaGanando auto) 

--PUNTO 2
--a)
correr :: Number-> Auto -> Auto
correr tiempo auto = auto {distanciaRecorrida = (distanciaRecorrida auto + tiempo) * velocidad auto} 

--b)
--i)
alterarVelocidad :: (Number -> Number) -> Auto -> Auto
alterarVelocidad modificadorVelocidad auto = auto { velocidad = modificadorVelocidad (velocidad auto)}
 
--ii)
bajarVelocidad:: Number -> Auto -> Auto
bajarVelocidad velocidadABajar = alterarVelocidad (max 0 . subtract velocidadABajar)

--PUNTO 3
afectarALosQueCumplen ::(a->Bool) -> (a->a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map  efecto. filter criterio) lista ++ filter (not.criterio) lista

type PowerUp = Auto -> Carrera ->Carrera

--a)
terremoto :: PowerUp
terremoto autoQueGatillo = afectarALosQueCumplen (estaCerca autoQueGatillo) (bajarVelocidad 50)

miguelito :: Number -> PowerUp
miguelito velocidadABajar autoQueGatillo = afectarALosQueCumplen (leVaGanando autoQueGatillo) (bajarVelocidad velocidadABajar)

jetPack :: Number -> PowerUp
jetPack tiempo autoQueGatillo = afectarALosQueCumplen (== autoQueGatillo)
        (alterarVelocidad (\ _ -> velocidad autoQueGatillo) . correr tiempo . alterarVelocidad (*2))

--PUNTO 4
type Color = String
type Puesto = Number
type Evento = Carrera -> Carrera

--a)
simularCarrera :: Carrera -> [Evento] -> [(Puesto, Color)]
simularCarrera = undefined

tablaDePosiciones :: Carrera -> [(Puesto, Color)]
tablaDePosiciones carrera 
  = map (entradaDeTabla carrera) carrera

entradaDeTabla :: Carrera -> Auto -> (Puesto, String)
entradaDeTabla carrera auto = (puesto auto carrera, color auto)

procesarEventos :: [Evento] -> Carrera -> Carrera
procesarEventos eventos carreraInicial =
    foldl (\carreraActual evento -> evento carreraActual) 
      carreraInicial eventos

--b)
--i)
correnTodos :: Number -> Evento
correnTodos tiempo = map (correr tiempo)

--ii)
usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp powerUp colorBuscado carrera = powerUp autoQueGatillaElPoder carrera
    where autoQueGatillaElPoder = find ((== colorBuscado).color) carrera

find :: (c -> Bool) -> [c] -> c
find cond = head . filter cond

