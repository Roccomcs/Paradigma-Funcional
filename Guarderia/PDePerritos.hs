{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

data Perrito = UnPerrito {
    raza :: String,
    juguetesFavoritos :: [Juguetes],
    tiempoEnGuarderia :: Int,
    energia :: Int
}

data Guarderia = UnaGuarderia {
    nombre :: String,
    rutina :: [Actividad]
}

type Juguetes = String
type Ejercicio = Perrito -> Perrito
type Actividad = (Ejercicio, Int)

ejercicio :: Actividad -> Ejercicio
ejercicio = fst

tiempoDeActividad :: Actividad -> Int
tiempoDeActividad = snd

-- Accesorios

mapRaza :: (String -> String) -> Perrito -> Perrito
mapRaza funcion unPerrito = unPerrito {raza = funcion . raza $ unPerrito}

mapJuguetes :: ([Juguetes] -> [Juguetes]) -> Perrito -> Perrito
mapJuguetes funcion unPerrito = unPerrito {juguetesFavoritos = funcion . juguetesFavoritos $ unPerrito}

mapTiempo :: (Int -> Int) -> Perrito -> Perrito
mapTiempo funcion unPerrito = unPerrito {tiempoEnGuarderia = funcion . tiempoEnGuarderia $ unPerrito}

mapEnergia :: (Int -> Int) -> Perrito -> Perrito
mapEnergia funcion unPerrito = unPerrito {energia = max 0 . funcion . energia $ unPerrito}

mapNombre :: (String -> String) -> Guarderia -> Guarderia
mapNombre funcion unaGuarderia = unaGuarderia {nombre = funcion . nombre $ unaGuarderia}

mapRutina :: ([Actividad] -> [Actividad]) -> Guarderia -> Guarderia
mapRutina funcion unaGuarderia = unaGuarderia {rutina = funcion . rutina $ unaGuarderia}

-- Secundarias

agregarJuguete :: Juguetes -> Perrito -> Perrito
agregarJuguete juguete = mapJuguetes (juguete :) 
-- * Lo agrega al principio de la lista

agregarJuguete2 :: Juguetes -> Perrito -> Perrito
agregarJuguete2 juguete  = mapJuguetes (++ [juguete]) 
-- * Lo agrega al final de la lista

regalar :: Juguetes -> Ejercicio
regalar = agregarJuguete 

permaneceAlMenos :: Int -> Perrito -> Bool
permaneceAlMenos minutos unPerrito = tiempoEnGuarderia unPerrito >= minutos 

esDeRazaExtravagante :: Perrito -> Bool
esDeRazaExtravagante unPerrito = raza unPerrito == "Dalmata" || raza unPerrito == "Pomerania"

perderPrimerJuguete :: Perrito -> Perrito
perderPrimerJuguete = mapJuguetes (drop 1)

-- Punto 1

jugar :: Ejercicio
jugar = mapEnergia (subtract 10) 

ladrar :: Int -> Ejercicio
ladrar cantidadDeLadridos = mapEnergia (+ div cantidadDeLadridos 2)

diaDeSpa :: Ejercicio
diaDeSpa unPerrito 
  |permaneceAlMenos 50 unPerrito || esDeRazaExtravagante unPerrito = mapEnergia (const 100) . agregarJuguete "Peine de goma" $ unPerrito
  |otherwise = unPerrito

diaDeCampo :: Ejercicio
diaDeCampo unPerrito = perderPrimerJuguete . jugar $ unPerrito
-- diaDeCampo = mapJuguetes tail . jugar

-- & ----------------------------------------------------------------------------------------------------------------------

zara :: Perrito
zara = UnPerrito "Dalmata" ["Pelota", "Mantita"] 90 80

guarderia :: Guarderia
guarderia = UnaGuarderia { 
  nombre = "Guardería PdePerritos",
  rutina = [
  (jugar, 30),
  (ladrar 18, 20),
  (regalar "Pelota", 0),
  (diaDeSpa, 0),
  (diaDeCampo, 0)
]
}


puedeEstarEnGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnGuarderia unPerrito unaGuarderia
  |tiempoEnGuarderia unPerrito > tiempoDeRutina unaGuarderia = True 
  |otherwise = False

tiempoDeRutina ::Guarderia -> Int
tiempoDeRutina unaGuarderia = sum . map tiempoDeActividad $ rutina unaGuarderia

perrosResponsables :: Perrito -> Bool
perrosResponsables unPerrito
  | tieneMasDeNJuguetes 3 . diaDeCampo $ unPerrito = True
  | otherwise = False

tieneMasDeNJuguetes :: Int -> Perrito -> Bool
tieneMasDeNJuguetes cantidad unPerrito = length (juguetesFavoritos unPerrito)  > cantidad
-- * tieneMasDeNJuguetes cantidad = (> cantidad) . length . juguetesFavoritos

realizarRutina :: Guarderia -> Perrito -> Perrito
realizarRutina unaGuarderia unPerrito
  |puedeEstarEnGuarderia unPerrito unaGuarderia = (componerRutina . rutina $ unaGuarderia) unPerrito
  |otherwise = unPerrito

componerRutina :: [Actividad] -> Ejercicio
componerRutina = foldr ((.) . ejercicio) id

perrosCansados :: [Perrito] -> Guarderia -> [Perrito]
perrosCansados listaPerritos unaGuarderia = filter (estaCansado . realizarRutina unaGuarderia) listaPerritos

estaCansado :: Perrito ->  Bool
estaCansado unPerrito = (<5) . energia $ unPerrito

perritoPi :: Perrito
perritoPi =  UnPerrito "Labrador" soguitasInfinitas 314 159

soguitasInfinitas :: [Juguetes]  
soguitasInfinitas = map (("Soguita " ++) . show) [1..]

-- * Preguntas
-- 1) Si, devuelve False ya que solo evaluamos la raza
-- 2) a_ Rompe por que tenemos que analizar todos los juguetes y nunca va a ser en la lista infinita
--    b_ En la guarderia de perritos le regalan una pelota, como lo ponemos al principio de la lista lo encontraria y puede terminar
--    c_ La encuentra, devuelve True
-- 3) No terminaria nunca en la consola ya que tiene infinitos ejercicios
-- 4) Se puede hacer pero nunca se termina de mostrar
