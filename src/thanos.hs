module Lib where
import PdePreludat

data Guante = Guante {
    material:: String,
    gemas :: [Gema]
} deriving (Show, Eq)

data Personaje = Personaje {
    edad :: Number,
    nombre :: String,
    energia:: Number,
    habilidades:: [String],
    planeta :: String
} deriving (Show, Eq)

type Universo = [Personaje]
--PRIMERA PARTE
--punto 1

guanteChasqueable :: Guante -> Bool
guanteChasqueable  guante = ((==6).length.gemas) guante &&  ((=="uru").material) guante

eliminarMitadPersonajes ::Universo -> Universo
eliminarMitadPersonajes uni = take (length uni / 2) uni

chasquido :: Guante -> Universo -> Universo
chasquido guante uni
    |guanteChasqueable guante = eliminarMitadPersonajes uni
    |otherwise = uni

--punto 2
aptoParaPendex :: Universo -> Bool
aptoParaPendex = any (\personaje -> edad personaje < 45) 

energiaTotal :: Universo -> Number
energiaTotal =sum.map energia.filter ((>1).(length.habilidades))

--SEGUNDA PARTE
--punto 3
type Gema = Personaje -> Personaje
mente :: Number -> Gema
mente = quitarEnergia 

alma :: String -> Gema
alma habilidadAEliminar = quitarEnergia 10.eliminarUnaHabilidad habilidadAEliminar

espacio :: Gema
espacio per = quitarEnergia 10 per {planeta = "Saturno"}

poder :: Gema
poder per = quitarHabilidades per {energia = 0}

tiempo :: Gema
tiempo = reducirEdad.quitarEnergia 50

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

reducirEdad :: Gema
reducirEdad per = per {edad = max 18 (edad per/2)}

quitarHabilidades :: Gema
quitarHabilidades per
    |length (habilidades per) <= 2 = per {habilidades = []}
    |otherwise = per

quitarEnergia :: Number -> Gema
quitarEnergia valorAQuitar personaje = personaje {energia = energia personaje - valorAQuitar }

eliminarUnaHabilidad :: String -> Gema
eliminarUnaHabilidad habilidadAEliminar per
    |elem habilidadAEliminar (habilidades per) = per {habilidades = filter (/= habilidadAEliminar) (habilidades per)}

--punto 5
utilizar :: [Gema] -> Gema
utilizar gemas personaje = foldl aplicarGema personaje gemas

aplicarGema :: Personaje -> Gema -> Personaje
aplicarGema per gema = gema per

--punto 6