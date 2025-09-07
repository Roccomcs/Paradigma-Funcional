{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.Char (toUpper, isUpper)

type Objeto = Barbaro -> Barbaro -- Creo types para cada lista
type Habilidades = String
type Evento = Barbaro -> Bool
type Aventura = [Evento]

data Barbaro = UnBarbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidades],
    objetos :: [Objeto]
    } 

dave :: Barbaro
dave = UnBarbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

-- Accesorios
mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre funcion unBarbaro = unBarbaro {nombre = funcion . nombre $ unBarbaro}

mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza funcion unBarbaro = unBarbaro {fuerza = funcion . fuerza $ unBarbaro}

mapHabilidades :: ([Habilidades] -> [Habilidades]) -> Barbaro -> Barbaro
mapHabilidades funcion unBarbaro = unBarbaro {habilidades = funcion . habilidades $ unBarbaro}

mapObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
mapObjetos funcion unBarbaro = unBarbaro {objetos = funcion . objetos $ unBarbaro}

-- Funciones secundarias

desaparecerObjetos :: Objeto
desaparecerObjetos unBarbaro = unBarbaro { objetos = [varitasDefectuosas]}

agregarHabilidad :: Habilidades -> Barbaro -> Barbaro
agregarHabilidad unaHabilidad unBarbaro = mapHabilidades (++[unaHabilidad]) unBarbaro

agregarHabilidad2 :: Habilidades -> Barbaro -> Barbaro
agregarHabilidad2 unaHabilidad unBarbaro = mapHabilidades (unaHabilidad :) unBarbaro

convertirAMayusculas :: [String] -> [String]
convertirAMayusculas listaHabilidades = map (map toUpper)  listaHabilidades 
-- * toUpper va de Char -> Char, debemos mapear el string

concatenar :: [String] -> [String]
concatenar listaHabilidades = [concat listaHabilidades] -- * concat devuelve un único string, como necesito la lista encierro todo

noTienePulgares :: String -> Bool
noTienePulgares "Faffy" = True
noTienePulgares "Astro" = True
noTienePulgares _ = False

noTienePulgares2 :: String -> Bool
noTienePulgares2 unNombre
  |unNombre == "Faffy" || unNombre == "Astro" = True
  |otherwise = False

saqueo :: Evento
saqueo unBarbaro = tieneHabilidad "robar" unBarbaro && esFuerte unBarbaro

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad habilidad unBarbaro = (elem habilidad.habilidades) unBarbaro 

esFuerte :: Barbaro -> Bool
esFuerte unBarbaro = (>80).fuerza $ unBarbaro

gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro  = poderDeGritoDeGuerra unBarbaro == cantidadDeLetrasDeHabilidades unBarbaro

poderDeGritoDeGuerra :: Barbaro -> Int
poderDeGritoDeGuerra  = (*4) . length . objetos

cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades unBarbaro = sum . map length.habilidades $ unBarbaro  

caligrafia :: Evento
caligrafia unBarbaro = all tieneMasDe3VocalesYEmpiezaConMayuscula (habilidades unBarbaro)

tieneMasDe3VocalesYEmpiezaConMayuscula :: String -> Bool
tieneMasDe3VocalesYEmpiezaConMayuscula habilidad = tieneMasDe3Vocales habilidad && empiezaConMayuscula habilidad

tieneMasDe3Vocales :: String -> Bool
tieneMasDe3Vocales habilidad = (>3) . length . filter esVocalEficiente $ habilidad  

esVocal :: Char -> Bool
esVocal 'a' = True
esVocal 'e' = True
esVocal 'i' = True
esVocal 'o' = True
esVocal 'u' = True
esVocal 'A' = True
esVocal 'E' = True
esVocal 'I' = True
esVocal 'O' = True
esVocal 'U' = True
esVocal _ = False

esVocalEficiente :: Char -> Bool
esVocalEficiente caracter = elem caracter "aeiouAEIOU"

empiezaConMayuscula :: String -> Bool
empiezaConMayuscula habilidad =  isUpper.head $ habilidad 

utilizarObjetos :: Barbaro -> Barbaro
utilizarObjetos unBarbaro = (foldr (.) id (objetos unBarbaro)) unBarbaro
-- utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

-- Punto 1

espadas :: Int -> Objeto
espadas pesoEspada unBarbaro = mapFuerza (+ pesoEspada * 2) unBarbaro

amuletosMisticos :: Habilidades -> Objeto
amuletosMisticos unaHabilidad unBarbaro= mapHabilidades (++[unaHabilidad]) unBarbaro 

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = agregarHabilidad "hacerMagia" . desaparecerObjetos $ unBarbaro

ardilla :: Objeto
ardilla = id 

cuerda :: Objeto -> Objeto -> Objeto
cuerda primerObjeto segundoObjeto unBarbaro = primerObjeto . segundoObjeto $ unBarbaro  

-- Punto 2

megafono :: Objeto
megafono unBarbaro = mapHabilidades (convertirAMayusculas . concatenar)  unBarbaro

megafonoBarbarico :: Objeto
megafonoBarbarico unBarbaro = cuerda ardilla megafono unBarbaro

-- Punto 3

sobreviveInvasionDeSuciosDuendes :: Evento
sobreviveInvasionDeSuciosDuendes unBarbaro = (elem "Escribir Poesia Atroz" . habilidades) unBarbaro

cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = (noTienePulgares . nombre) unBarbaro
  
ritualDeFechorias :: [Evento] -> Evento
ritualDeFechorias eventos unBarbaro = pasaUnaPrueba eventos unBarbaro 

pasaUnaPrueba :: [Evento] -> Barbaro -> Bool
pasaUnaPrueba eventos unBarbaro = any ($ unBarbaro) eventos

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro] 
sobrevivientes unosBarbaros unaAventura =  filter (pasaTodasLasPruebas unaAventura) unosBarbaros

pasaTodasLasPruebas :: [Evento] -> Barbaro -> Bool
pasaTodasLasPruebas eventos unBarbaro = all ($ unBarbaro) eventos

-- Punto 4

sinRepetidos :: (Eq a) => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (cabeza : cola)
  | elem cabeza cola = sinRepetidos cola
  | otherwise = cabeza : sinRepetidos cola

descendiente :: Barbaro -> Barbaro
descendiente unBarbaro = utilizarObjetos . mapNombre (++ "*") . mapHabilidades sinRepetidos $ unBarbaro

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = iterate descendiente unBarbaro 
