module Lib where
import PdePreludat

--parte A
data Perro = Perro{
    raza:: String,
    juguetesFav :: [String],
    estadia :: Number,
    energia :: Number
} deriving (Show, Eq)

data Guarderia = Guarderia {
    nombre :: String,
    rutina :: [Actividad]
} deriving (Show, Eq)

data Actividad = Actividad {
    duracion :: Number,
    ejercicio:: Ejercicio
}deriving (Show, Eq)

type Ejercicio = Perro -> Perro

cambiarEnergia :: Number -> Perro -> Perro
cambiarEnergia energiaAAgregar perro = perro {energia = max 0 (energia perro + energiaAAgregar)}

asignarEnergia :: Number -> Perro -> Perro
asignarEnergia energiaAAsignar perro = perro {energia = energiaAAsignar}

esRazaExtravagamente :: Perro -> Bool
esRazaExtravagamente perro = raza perro == "Dalmata" || raza perro == "Pomerania"


jugar :: Ejercicio
jugar = cambiarEnergia (-10)

ladrar ::Number -> Ejercicio
ladrar ladridos = cambiarEnergia (ladridos/2)

regalar :: String -> Ejercicio
regalar juguete perro = perro {juguetesFav = juguete:juguetesFav perro} 

diaSpa :: Ejercicio
diaSpa perro
    |esRazaExtravagamente perro && (estadia perro > 50)= (asignarEnergia 100.regalar "Peine de goma") perro
    |otherwise = perro

diaCampo :: Ejercicio
diaCampo perro = perro {juguetesFav = tail (juguetesFav perro)}

zara :: Perro
zara = Perro "Dalmata" ["Mantita","Pelota"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = Guarderia "P de Perritos" [Actividad 30 jugar, Actividad 20 (ladrar 18), Actividad 0 (regalar "Pelota"),Actividad 120 diaSpa, Actividad 720 diaCampo]

--parte B
duracionRutina :: [Actividad] -> Number
duracionRutina = sum.map duracion

puedeEstar :: Guarderia -> Perro -> Bool
puedeEstar guarderia perro = estadia perro > duracionRutina (rutina guarderia)

perrosResponsables :: [Perro] -> [Perro]
perrosResponsables = filter esReponsable

esReponsable :: Perro -> Bool
esReponsable perro = (length.juguetesFav.diaCampo) perro > 3

hacerRutina :: Guarderia -> Perro -> Perro
hacerRutina guarderia perro
    |puedeEstar guarderia perro = foldl hacerActividad perro (rutina guarderia)
    |otherwise = perro

hacerActividad ::Perro -> Actividad -> Perro
hacerActividad perro act = ejercicio act perro

perrosCansados :: Guarderia -> [Perro] -> [Perro]
perrosCansados guarderia = filter (seCansaPerro guarderia)

seCansaPerro :: Guarderia -> Perro -> Bool
seCansaPerro guarderia perro = (energia.hacerRutina guarderia) perro < 5

--parte C
perropi :: Perro
perropi = Perro "Labrador" soguitasInfitas 314 159

soguitasInfitas :: [String]
soguitasInfitas = "Soguita":map (("Soguita"++).(" " ++).show) [1..]

tieneJuguete :: String -> Perro -> Bool
tieneJuguete jug perro = elem jug (juguetesFav perro)

-- > 1. esRazaExtravagamente pi
-- 2.a) no termina b) no termina c) si
-- 3. si
-- 4. se lo agrega al principio
