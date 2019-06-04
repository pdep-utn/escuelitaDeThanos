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
} deriving (Show)

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show)

{- Punto 1
Hacer chasquido 
  *Si el guante está completo (6 gemas) y es de material "uru"
  eliminar a la mitad del universo
-}
type Universo = [Personaje]

chasquear :: Guantelete -> Universo -> Universo
chasquear guantelete universo | puedeUsarse guantelete =  reducirMitad universo 
 | otherwise = universo 

reducirMitad :: Universo -> Universo
reducirMitad universo = take (length universo `div` 2) universo 

puedeUsarse:: Guantelete -> Bool
puedeUsarse guantelete = ((==6).length.gemas) guantelete && ((=="uru").material) guantelete

{- Punto 2 Modelar las gemas:

mente = debilita energía del usuario
-} 
mente :: Float -> Gema
mente valor personaje = personaje {
  energia = energia personaje - valor
}
{-
el alma = elimina habilidad enemigo
-}
alma :: String -> Gema
alma habilidad personaje = mente 10 personaje {
  habilidades = filter (/=habilidad) $ habilidades personaje 
}
{-
el espacio = mover a otro universo -}
espacio :: String -> Gema
espacio nuevoPlaneta personaje = mente 20 personaje {
  planeta = nuevoPlaneta
}
{-
el poder = deja sin energia al rival y si tiene mas de 2 habilidades o menos se las quita -}
poder :: Gema
poder personaje = atacarHabilidades.mente (energia personaje) $ personaje

atacarHabilidades personaje | (<=2).length.habilidades $ personaje = quitarHabilidades personaje
 | otherwise = personaje   

quitarHabilidades:: Gema
quitarHabilidades personaje = personaje {
  habilidades = []
}
{-
el tiempo = cambia la edad a la mitad
-}
tiempo :: Gema
tiempo personaje = mente 50 personaje {
  edad = (max 18.div (edad personaje)) 2 
}

{-realidad 
Puede ejecutar cualquier deseo del usuario. Si el deseo es correcto lo puede ejecutar, en caso contrario todo queda igual 
  -}

realidad :: Deseo -> Gema
realidad deseo personaje | esDigno deseo personaje = deseo personaje
 | otherwise = personaje  

esDigno :: Deseo -> Personaje -> Bool
esDigno deseo personaje  = (energia personaje) > (energia.deseo) personaje

realidadLoca personaje = realidad (\personaje -> personaje {energia = 3}) personaje

{- Punto 3
usar guante contra un personaje => ejecuta las gemas que tiene 
-}
usar :: [Gema] -> Gema
usar gemas destinatario = foldr ($) destinatario $ gemas  

-- Punto 4 orden superior! 

{- Saber si un universo es de jovatos, que ocurre si todos los personajes que lo integran tienen más de 70 años.-}

universoDeJovatos :: Universo -> Bool 
universoDeJovatos = all $ (>=70).edad


{-Saber la energía total de un universo que es la sumatoria de todas las energías de sus integrantes que tienen más de una habilidad.-}
energiaTotalDelUniverso :: Universo -> Float 
energiaTotalDelUniverso = sum.map energia.filter ((>0).length.habilidades)

{-
Gema más poderosa
-}

gemaPoderosa :: Personaje -> Guantelete -> Gema
gemaPoderosa personaje guantelte = gemaMasPoderosaDe personaje $ gemas guantelte

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe personaje (gema1:gema2:gemas) 
    | (energia.gema1) personaje > (energia.gema2) personaje = gemaMasPoderosaDe personaje (gema1:gemas)
    | otherwise = gemaMasPoderosaDe personaje (gema2:gemas)


{- Punto 6 evaluación diferida-}
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" $ infinitasGemas tiempo

punisher:: Personaje 
punisher = Personaje "The Punisher" ["Disparar con de todo","golpear"] "Tierra" 38 350.0

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelte = usar $ (take 3 .gemas) guantelte

-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher
