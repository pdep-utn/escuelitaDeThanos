import PdePreludat
import Library
import Test.Hspec

spiderMan :: Personaje
spiderMan = Personaje{
    edad = 21,
    energia = 2300,
    habilidades = ["sentido arácnido", "sacar fotos", "trepar paredes"],
    nombre = "Peter Parker",
    planeta = "tierra"
}    

ironMan :: Personaje
ironMan = Personaje{
    edad = 48,
    energia = 1900,
    habilidades = ["volar", "programacion en Haskell", "programacion en Prolog"],
    nombre = "Tony Stark",
    planeta = "tierra"
}     

thor :: Personaje
thor = Personaje{
    edad = 1500,
    energia = 3000,
    habilidades = ["usar Mjolnir"],
    nombre = "Thor",
    planeta = "asgard"
}

drStrange :: Personaje
drStrange = Personaje{
    edad = 50,
    energia = 1000,
    habilidades = ["levitar", "ver futuros"],
    nombre = "Stephen Strange",
    planeta = "tierra"
}

universo :: Universo
universo = [ironMan, thor, drStrange]

guanteleteIncompleto :: Guantelete
guanteleteIncompleto = Guantelete {
  material = "uru",
  gemas = [mente 10, alma "usar Mjolnir"]
}

guanteletePosta :: Guantelete
guanteletePosta = guanteleteIncompleto {
  gemas = [mente 10, alma "usar Mjolnir",espacio "Jupiter", poder, tiempo, gemaLoca poder]
}


guanteleteDeMadera :: Guantelete
guanteleteDeMadera = guanteletePosta {
  material = "Quebracho colorado"
}

{-
Punto 4! 
Dar un ejemplo de un guantelete de goma con las gemas tiempo, 
alma que quita la habilidad de “usar Mjolnir”
y la gema loca que manipula el poder del alma tratando de eliminar la “programación en Haskell”.
-}
guanteleteDeGoma :: Guantelete
guanteleteDeGoma = Guantelete {
  material = "100% Acrilo Nitrilo",
  gemas = [mente 10, alma "usar Mjolnir",espacio "Jupiter", poder, tiempo, gemaLoca poder]
}

main :: IO ()
main = hspec $ do

  describe "Test de chasquido" $ do
    it "Si un guantelete está completo y su material es Uru entonces reduce a la mitad el universo" $ do
      chasquear guanteletePosta universo `shouldBe` [ironMan]
    it "Si un guantelete no está completo pero su material es correcto entonces el universo queda igual" $ do
      chasquear guanteleteIncompleto universo `shouldBe` [ironMan, thor, drStrange]
    it "Si un guantelete no es de material correcto pero está completo entonces el universo queda igual" $ do
      chasquear guanteleteDeMadera universo `shouldBe` [ironMan, thor, drStrange]
  
  describe "Test de orden superior" $ do 
    it "Si un universo no tiene personajes menores a 45 años entonces no es apto para pendex" $ do 
      universo `shouldNotSatisfy` universoAptoParaPendex
    it "Si un universo tiene al menos un personaje menores a 45 años entonces es apto para pendex" $ do 
      universo ++ [spiderMan] `shouldSatisfy` universoAptoParaPendex
    it "Si un universo tiene personajes de más de una habilidad entonces la energia total es la suma de ellos" $ do 
      energiaTotalDelUniverso universo `shouldBe` 2900

  describe "Test de las gemas" $ do 
    it "Si el personaje se ve atacado por la gema de la mente entonces reduce su energía en un valor determinado" $ do 
      (energia.mente 100) drStrange `shouldBe` 900
    it "Si el personaje es atacado por la gema del alma con una habilidad que posee entonces se la remueve" $ do
      (habilidades.alma "usar Mjolnir") thor `shouldBe` []
    it "Si el personaje es atacado por la gema del alma con una habilidad que no posee entonces conserva las mismas habilidades" $ do
      (habilidades.alma "usar Mjolnir") drStrange `shouldMatchList` ["levitar", "ver futuros"]
    it "Si el personaje es atacado por la gema del alma entonces reduce su energia en 10 unidades" $ do
      (energia.alma "usar Mjolnir") thor `shouldBe` 2990
    it "Si el personaje es atacado por la gema del espacio entonces cambia al planeta por el designado en la gema" $ do
      (planeta.espacio "Naboo") spiderMan `shouldBe` "Naboo"
    it "Si el personaje es atacado por la gema del espacio entonces resta 20 unidades en su energía" $ do
      (energia.espacio "Naboo") spiderMan `shouldBe` 2280
    it "Si el personaje es atacado por la gema del poder entonces deja sin energía al oponente" $ do 
      (energia.poder) spiderMan `shouldBe` 0
    it "Si el personaje es atacado por la gema del poder y tiene más de dos habilidades entonces las conserva" $do
      (habilidades.poder) ironMan `shouldMatchList` ["volar", "programacion en Haskell", "programacion en Prolog"]
    it "Si el personaje es atacado por la gema del poder y tiene dos o menos habilidades entonces las pierde" $do
      (habilidades.poder) drStrange `shouldBe` []
    it "Si un personaje es atacado por la gema del tiempo y tiene una edad de más del doble de 18 años entonces queda con la mitad de su edad" $ do 
      (edad.tiempo) drStrange `shouldBe` 25
    it "Si un personaje es atacado por la gema del tiempo y tiene una edad menoral doble de 18 años entonces queda con 18 años de edad" $ do 
      (edad.tiempo) spiderMan `shouldBe` 18
    it "Si un personaje es atacado por la gema del tiempo entonces resta 50 puntos de energía" $ do 
      (energia.tiempo) spiderMan `shouldBe` 2250
    it "Si un personaje es atacado por la gema loca entonces aplica dos veces su poder" $ do 
      (habilidades.gemaLoca (alma "usar Mjolnir")) thor `shouldBe` []
      (energia.gemaLoca (alma "usar Mjolnir")) thor `shouldBe` 2980
  
  describe "Test de uso de gemas" $ do 
    it "Dada una lista de gemas que se usa sobre un personaje entonces se modifica el mismo con el efecto individual de cada una" $ do
      (energia.usar [alma "usar Mjolnir", mente 100, espacio "XYZ"]) thor `shouldBe` 2870
      (planeta.usar [alma "usar Mjolnir", mente 100, espacio "XYZ"]) thor `shouldBe` "XYZ"
      (habilidades.usar [alma "usar Mjolnir", mente 100, espacio "XYZ"]) thor `shouldBe` []
  
  describe "Testeo de gema más poderosa" $ do
    it "Dada una lista de gemas entonces se obtiene la más poderosa" $ do 
      (energia.(gemaMasPoderosa drStrange guanteleteDeGoma)) drStrange `shouldBe` 0 --la gema del poder quita los poderes
  