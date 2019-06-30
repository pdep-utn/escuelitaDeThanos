module Library where
import PdePreludat

type Gema = Personaje -> Personaje 
type Deseo = Personaje -> Personaje 


data Personaje = Personaje {
  nombre::String,
  habilidades::[String],
  planeta:: String, 
  edad::Int,
  energia::Float
} deriving (Eq, Show)

type Universo = [Personaje]

{- Alternativa
type Material = String
type Guantelete = (Material,[Gema])
-}

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show)

{- Punto 1
Hacer chasquido 
-}


chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo | puedeUsarse guantelete =  reducirMitad universo 
 | otherwise = universo 

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo 

puedeUsarse:: Guantelete -> Bool
puedeUsarse guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete

-- Punto 2 orden superior! 

universoAptoParaPendex :: Universo -> Bool 
universoAptoParaPendex = any $ (<=45).edad

energiaTotalDelUniverso :: Universo -> Float 
energiaTotalDelUniverso = sum.map energia.filter ((>1).length.habilidades)

{- Punto 3 Modelar las gemas -} 
mente :: Float -> Gema
mente = quitarEnergia

quitarEnergia :: Float -> Gema
quitarEnergia valor personaje = personaje {
  energia = energia personaje - valor
}

alma :: String -> Gema
alma habilidad personaje = quitarEnergia 10 personaje {
  habilidades = filter (/=habilidad) $ habilidades personaje 
}

espacio :: String -> Gema
espacio nuevoPlaneta personaje = quitarEnergia 20 personaje {
  planeta = nuevoPlaneta
}

poder :: Gema
poder personaje = atacarHabilidades.quitarEnergia (energia personaje) $ personaje

atacarHabilidades :: Personaje -> Personaje
atacarHabilidades personaje | (<=2).length.habilidades $ personaje = quitarHabilidades personaje
 | otherwise = personaje   

quitarHabilidades:: Gema
quitarHabilidades personaje = personaje {
  habilidades = []
}

tiempo :: Gema
tiempo personaje = quitarEnergia 50 personaje {
  edad = (max 18.div (edad personaje)) 2 
}

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema 


{- Punto 5
usar varias gemas contra un personaje.
-}
usar :: [Gema] -> Gema
usar listaDeGemas destinatario = foldr ($) destinatario $ listaDeGemas  

{- Punto 6
Gema más poderosa
-}

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa personaje guantelte = gemaMasPoderosaDe personaje $ gemas guantelte

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:gemas) 
    | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:gemas)
    | otherwise = gemaMasPoderosaDe personaje (gema2:gemas)


{- Punto 7 evaluación diferida -}
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" $ infinitasGemas tiempo

punisher:: Personaje 
punisher = Personaje "The Punisher" ["Disparar con de todo","golpear"] "Tierra" 38 350.0

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (usar . take 3. gemas) guantelete

-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher
