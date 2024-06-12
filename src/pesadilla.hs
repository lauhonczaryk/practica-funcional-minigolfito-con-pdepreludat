module Lib where
import PdePreludat

data Persona = UnaPersona{
    nombre:: String,
    recuerdos:: [String]
}

suki :: Persona
suki = UnaPersona {nombre = "Susana Kitimporta",recuerdos = ["abuelita", "escuela primaria", "examen aprobado","novio"]}

type Pesadilla = [String] -> [String]

--arrancan en 0
encontrarXPos :: Number -> [String] -> String
encontrarXPos pos = head.drop pos

listaFiltrada :: String -> [String]-> [String]
listaFiltrada string = filter (/= string)

ubicarEnPos :: Number -> String -> [String] -> [String]
ubicarEnPos pos s lista = take pos (listaFiltrada s lista) ++ [s] ++ drop pos (listaFiltrada s lista)

--pesadilla de movimiento
mover :: Number -> Number -> Pesadilla
mover n1 n2 = ubicarSegundo n2 n1.ubicarPrimero n1 n2

ubicarPrimero :: Number -> Number -> Pesadilla
ubicarPrimero n1 n2 recuerdos = ubicarEnPos n2 (encontrarXPos n1 recuerdos) recuerdos

ubicarSegundo:: Number -> Number -> Pesadilla
ubicarSegundo n1 n2 recuerdos = ubicarEnPos n1 (encontrarXPos n2 recuerdos) recuerdos

--pesadilla de sustitucion
reemplazar :: Number -> String -> Pesadilla
reemplazar pos string = ubicarEnPos pos string

--pesadilla desmorizadora
quitar :: String -> Pesadilla
quitar string recuerdos
    |elem string recuerdos = filter (/= string) recuerdos
    |otherwise = recuerdos

--suenio
nop :: Pesadilla
nop recuerdos = recuerdos

--punto 1
aplicarPesadilla :: Pesadilla -> Persona -> Persona
aplicarPesadilla pesadilla = pesadilla.recuerdos 

noche :: Persona -> [Pesadilla] -> Persona
noche persona = foldr (aplicarPesadilla) persona


