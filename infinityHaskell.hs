-- * [1]
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use $" #-}

data Personaje = UnPersonaje {
    nombre :: String,
    cantidadDePoder :: Int,
    listaDeDerrotas :: [Derrota],
    equipamientos :: [Equipamiento]
}

type Derrota = (String, Int)
type Equipamiento = Personaje -> Personaje

mapNombre :: (String -> String) -> Personaje -> Personaje
mapNombre funcion unPersonaje = unPersonaje {nombre = funcion . nombre $ unPersonaje}

mapCantidadDePoder :: (Int -> Int) -> Personaje -> Personaje
mapCantidadDePoder funcion unPersonaje = unPersonaje {cantidadDePoder = funcion . cantidadDePoder $ unPersonaje}

mapListaDeDerrotas :: ([Derrota] -> [Derrota]) -> Personaje -> Personaje
mapListaDeDerrotas funcion unPersonaje = unPersonaje {listaDeDerrotas = funcion . listaDeDerrotas $ unPersonaje}

mapEquipamientos :: ([Equipamiento] -> [Equipamiento]) -> Personaje -> Personaje
mapEquipamientos funcion unPersonaje = unPersonaje {equipamientos = funcion . equipamientos $ unPersonaje}

--  Personajes de prueba 
scarletWitch :: Personaje
scarletWitch = UnPersonaje "Scarlet Witch" 250 [] []

capitanAmerica :: Personaje
capitanAmerica = UnPersonaje "Capitan America" 345 [] []

blackWidow :: Personaje
blackWidow = UnPersonaje "Black Widow" 300 [] []

vision :: Personaje
vision = UnPersonaje "Vision" 270 [] []

ironMan :: Personaje
ironMan = UnPersonaje "Iron Man" 350 [("Hijo de Thanos", 2018)] []

spiderman :: Personaje
spiderman = UnPersonaje "Spiderman" 285 [] []

equipoA :: [Personaje]
equipoA = [scarletWitch,capitanAmerica, blackWidow]

equipoB :: [Personaje]
equipoB = [vision, ironMan, spiderman]
-- 

-- * [2]

entrenarUnPersonaje :: Int -> Personaje -> Personaje
entrenarUnPersonaje multiplicadorDePoder = mapCantidadDePoder (* multiplicadorDePoder)

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento unosPersonajes = map (entrenarUnPersonaje . length $ unosPersonajes) unosPersonajes

-- * [3]

esPoderoso :: Personaje -> Bool
esPoderoso unPersonaje = (>500) . cantidadDePoder $ unPersonaje

obtenerNombreDeDerrotas :: Personaje -> [String]
obtenerNombreDeDerrotas unPersonaje = map fst.listaDeDerrotas $ unPersonaje

derrotoA :: String -> Personaje -> Bool
derrotoA unEnemigo unPersonaje = elem unEnemigo (obtenerNombreDeDerrotas unPersonaje)

esDigno :: Personaje -> Bool
esDigno unPersonaje = esPoderoso unPersonaje && derrotoA "Hijo de Thanos" unPersonaje

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos = filter esDigno.entrenamiento

-- * [4]

esMasFuerteQue :: Personaje -> Personaje -> Bool
esMasFuerteQue unPersonaje otroPersonaje = cantidadDePoder unPersonaje > cantidadDePoder otroPersonaje 

agregarDerrota :: String -> Int -> Personaje -> Personaje
agregarDerrota unNombre unAño = mapListaDeDerrotas ((unNombre, unAño):)

peleaEntre :: Int -> Personaje -> Personaje -> Personaje
peleaEntre unAño unPersonaje otroPersonaje
  | esMasFuerteQue unPersonaje otroPersonaje = agregarDerrota (nombre otroPersonaje) unAño unPersonaje
  | otherwise = agregarDerrota (nombre unPersonaje) unAño otroPersonaje

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil unAño unEquipo otroEquipo = zipWith (peleaEntre unAño) unEquipo otroEquipo

-- * [2]

cantidadDeDerrotas :: Personaje -> Int
cantidadDeDerrotas = length.listaDeDerrotas

escudo :: Equipamiento
escudo unPersonaje
  | cantidadDeDerrotas unPersonaje < 5 = mapCantidadDePoder (+50) unPersonaje
  | otherwise = mapCantidadDePoder (subtract 100) unPersonaje

ironizarPersonaje :: Int -> Personaje -> String
ironizarPersonaje unaVersion unPersonaje = "Iron " ++ nombre unPersonaje ++ " V" ++ show unaVersion

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado unaVersion unPersonaje = mapNombre (const (ironizarPersonaje unaVersion unPersonaje)) unPersonaje
{- trajeMecanizado unaVersion unPersonaje = 
mapNombre (\nombre -> "Iron " ++ nombre ++ " V" ++ show unaVersion) 
unPersonaje -}

-- * [3.a]

equipamientoExclusivo :: String -> Personaje -> Equipamiento -> Personaje
equipamientoExclusivo unNombre unPersonaje unEquipamiento
  | unNombre == nombre unPersonaje = unEquipamiento unPersonaje
  | otherwise = unPersonaje

diosDelTrueno :: Personaje -> Personaje
diosDelTrueno = mapNombre (++ "dios del trueno")

diosBondadoso :: Personaje -> Personaje
diosBondadoso = mapListaDeDerrotas (const [])

stormBreaker :: Equipamiento
stormBreaker unPersonaje = equipamientoExclusivo "Thor" unPersonaje (diosDelTrueno.diosBondadoso)

stormBreaker2 :: Equipamiento
stormBreaker2 unPersonaje
  | nombre unPersonaje == "Thor" = diosDelTrueno (diosBondadoso unPersonaje)
  | otherwise = unPersonaje

-- * [3.b]

extrasInfinitos :: [String]
extrasInfinitos = map (("extra numero " ++) . show) [1..]

derrotasInfinitas :: [Derrota]
derrotasInfinitas = zip extrasInfinitos [2025..]

gemaDelAlma :: Equipamiento
gemaDelAlma unPersonaje = equipamientoExclusivo "Thanos" unPersonaje (mapListaDeDerrotas (++ derrotasInfinitas))

-- * [3.c]

-- Funcion que te dá el enunciado:
esGemaDelInfinito :: Equipamiento -> Bool
esGemaDelInfinito unEquipamiento = True

usarGemasDelInfinito :: Personaje -> Personaje
usarGemasDelInfinito unPersonaje = foldr (.) id (filter esGemaDelInfinito.equipamientos $ unPersonaje) unPersonaje

guanteleteDelInfinito :: Equipamiento
guanteleteDelInfinito unPersonaje = equipamientoExclusivo "Thanos" unPersonaje usarGemasDelInfinito

-- ^ ----------------------------------- PARTE C -----------------------------------
{- a. Si Black Widow utiliza el escudo, por Lazy Evaluation, 
la función restara 100 de poder a Black Widow.
b. Depende, si tiene menos de 500 de poder entonces obtendré False, por Lazy 
Evaluation. 
En cambio, si tiene más de 500 de poder, y "Hijo de Thanos" esta en su lista
de derrotas 
entonces devolverá True, si no está entonces la función entra en un bucle 
infinito.
c. Si se puede, ya que por Lazy Evaluation, agarra las primears 100 derrotas 
y finaliza. -}