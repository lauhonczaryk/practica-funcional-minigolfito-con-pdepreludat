module Lib where
import PdePreludat

data Investigador = Investigador {
    nombre:: String,
    cordura :: Number,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving (Show, Eq)

data Item = Item {
    nombreItem :: String,
    valor:: Number
} deriving (Show, Eq)


--punto 1
enloquecer :: Number -> Investigador -> Investigador
enloquecer ptos investigador = investigador {cordura = max 0 (cordura investigador - ptos)}

hallarItem :: Item -> Investigador -> Investigador
hallarItem item = enloquecer (valor item).sumarItem item 

sumarItem :: Item -> Investigador -> Investigador
sumarItem item investigador = investigador {items = item:items investigador}

--punto 2
algunoTieneItem :: Item -> [Investigador] -> Bool
algunoTieneItem item = any (elem item . items) 

--punto 3
lider :: [Investigador] -> Investigador
lider = maximoSegun potencial

potencial :: Investigador -> Number
potencial investigador 
    |(not.estaLoco) investigador = cordura investigador * ((+1).(*3).length.sucesosEvitados) investigador + valorItemMax investigador
    |otherwise = 0

estaLoco :: Investigador -> Bool
estaLoco investigador = cordura investigador == 0

valorItemMax :: Investigador -> Number
valorItemMax = maximum . map valor . items

--punto 4
deltaSegun :: (b -> Number) -> (b -> b) -> b -> Number
deltaSegun ponderacion transformacion valor = abs ((ponderacion . transformacion) valor - ponderacion valor)

--a
deltaCorduraTotal :: Number -> [Investigador] -> Number
deltaCorduraTotal ptos = sum.map (deltaSegun cordura (enloquecer ptos))

--b
--whaaaaat
deltaPotencial :: [Investigador] -> Number
deltaPotencial ivestigadores = undefined

--punto 5
data Suceso = Suceso {
    descripcion :: String,
    evitar :: [Investigador] -> Bool,
    consecuencias :: [Investigador] -> [Investigador]
}

despertar :: Suceso
despertar = Suceso "Despertar de un antiguo" (algunoTieneItem (Item "Necronomicon" 0)) (enloquecerTodos 10.tail)

ritual :: Suceso
ritual = Suceso "Ritual en Innsmouth" ((>100).potencial.lider) (enloquecerTodos 10.enfrentarSuceso despertar.(\investigadores -> sumarItemPrimero investigadores:tail investigadores))

enloquecerTodos :: Number -> [Investigador] -> [Investigador]
enloquecerTodos = map.enloquecer

sumarItemPrimero :: [Investigador] -> Investigador
sumarItemPrimero = sumarItem (Item "Daga maldita" 3).head

--punto 6
enfrentarSuceso :: Suceso -> [Investigador] -> [Investigador]
enfrentarSuceso suceso = enloquecerTodos 1.evitaSuceso suceso

evitaSuceso :: Suceso -> [Investigador] -> [Investigador]
evitaSuceso suceso investigadores
    |evitar suceso investigadores = map (sumarSuceso (descripcion suceso)) investigadores
    |otherwise = consecuencias suceso investigadores

sumarSuceso :: String -> Investigador -> Investigador
sumarSuceso suceso investigador = investigador {sucesosEvitados = suceso:sucesosEvitados investigador}

--punto 7
maximoSegun :: Ord a1 => (a2 -> a1) -> [a2] -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
    |f a > f b = a
    | otherwise =b


--ni idea como hacerlo
funcion :: [Investigador] -> Number -> Suceso -> Number
funcion investigadores ptos suceso = deltaCorduraTotal ptos (enfrentarSuceso suceso investigadores) 

deltaQueMasAfecta :: [Investigador] -> [Suceso] -> Number -> Suceso
deltaQueMasAfecta invest sucesos ptos = maximoSegun (funcion invest ptos) sucesos