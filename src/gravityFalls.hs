module Lib where
import PdePreludat

data Persona = Persona{
    edad :: Number,
    items :: [String],
    experiencia :: Number
}

data Criatura = Criatura {
    peligrosidad :: Number,
    deshacerse :: Persona -> Bool
}

siempreDetras :: Criatura
siempreDetras = Criatura 0 (\_ -> False)

gnomos :: Number -> Criatura
gnomos cantidad = Criatura (2 ^ cantidad) (tieneItem "Soplador de hojas")

fantasma :: Number -> (Persona -> Bool) -> Criatura
fantasma categoria asuntoPendiente = Criatura (categoria*20) asuntoPendiente

enfrentarCriatura :: Criatura -> Persona -> Persona
enfrentarCriatura criatura persona
    |deshacerse criatura persona = persona {experiencia = peligrosidad criatura}
    |otherwise = persona {experiencia = experiencia persona + 1}

experienciaCapazGanar :: Persona -> [Criatura] -> Number
experienciaCapazGanar persona = experiencia.foldl (flip enfrentarCriatura) persona

tieneItem:: String -> Persona -> Bool
tieneItem item persona = elem item (items persona)
--criaturasEjemplo ::[Criatura]
--criaturasEjemplo = [siempreDetras, gnomos 10, fantasma 3 (((\persona -> edad persona) < 13) && elem "Disfraz de oveja" (\persona -> items persona)), fantasma 1 (experiencia persona > 10)]

-- > experienciaCapazGanar persona criaturasEjemplo