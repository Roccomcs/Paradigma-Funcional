import Data.Time (CalendarDiffDays)
-- & Tupla de dos enteros
-- & La tupla de dos enteros es una forma de representar una fecha, donde el primer entero es el dia y el segundo entero es el mes.
type Fecha = (Int, Int) -- (Dia, Mes)
type Festejo = (String, Fecha, String, Int) -- (Nombre, Fecha, Lugar, Cantidad de regalos)

-- ~ El festejoDeGus es una tupla que representa al cumpleaños de Gus, que es el 8 de marzo, en El Bolson, con 314 regalos.
-- ~ El festejoDeVicky es una tupla que representa al cumpleaños de Vicky, que es el 13 de abril, en Plaza, con 1000 regalos.
festejoDeGus :: Festejo
festejoDeGus = ("Gus", (8,2), "El Bolson", 314)

festejoDeVicky :: Festejo
festejoDeVicky = ("Vicky", (13,4), "Plaza", 1000)

festejoDeDia :: Festejo
festejoDeDia = ("Dia", (14,4), "Aula", 250)

festejoDeRo :: Festejo
festejoDeRo = ("Ro", (14,4), "Aula", 31)

festejoDeOrne :: Festejo
festejoDeOrne = ("Orne", (26,4), "Parque Aereo", 50)

festejoDeLu :: Festejo
festejoDeLu = ("Lu", (2,9), "Casa", 150)

festejoDeNacho:: Festejo
festejoDeNacho = ("Nacho", (28,9), "Sala de escape", 22)

festejoDePetru :: Festejo
festejoDePetru = ("Petru", (28,5), "Tecno", 24)

-- ~ Esta funcion recibe un festejo y devuelve el nombre de la persona que festeja.
personaQueFesteja :: Festejo -> String
personaQueFesteja (nombre, _, _, _) = nombre

-- ~ Esta funcion recibe un festejo y devuelve la fecha de cuando se festeja.
fechaDeFestejo :: Festejo -> Fecha
fechaDeFestejo (_, fecha, _, _) = fecha

-- ~ Esta funcion recibe un festejo y devuelve el lugar donde se festeja.
lugarDeFestejo :: Festejo -> String
lugarDeFestejo (_, _, lugar, _) = lugar 

-- ~ Cantidad de regalos: Esta funcion recibe un festejo y devuelve la cantidad de regalos que tiene. 
cantidadDeRegalos :: Festejo -> Int
cantidadDeRegalos (_, _, _, cantidad) = cantidad
-- ? Esta es otra forma de escribir la misma funcion, pero con un patron de tupla diferente.
-- cantidadDeRegalos ("Nombre", fecha, "lugar", cantidad) = cantidad
-- cantidadDeRegalos ("Gus", fecha, "El Bolson", cantidad) = cantidad

diaDeFestejo :: Festejo -> Int
diaDeFestejo (_, (dia, _), _, _) = dia

-- & Esta es otra forma de escribir la misma funcion, pero con un patron de tupla diferente.
diaDeFestejo2 :: Festejo -> Int
diaDeFestejo2 festejo = fst (fechaDeFestejo festejo)

{-  Lo vimos en clase pero me parece que esta mal 
diaDeFestejo2 :: Festejo -> dia
diaDeFestejo2 Festejo (fst.fechaDeFestejo) Festejo
-}

mesDeFestejo :: Festejo -> Int
mesDeFestejo (_, (_, mes), _, _) = mes

-- & Esta es otra forma de escribir la misma funcion, pero con un patron de tupla diferente.
mesDeFestejo2 :: Festejo -> Int
mesDeFestejo2 festejo = snd (fechaDeFestejo festejo)

-- & Lista de festejos: Esta funcion devuelve una lista de festejos, que son tuplas que representan los cumpleaños
festejos :: [Festejo]
festejos = [festejoDeGus, festejoDeVicky, festejoDeDia, festejoDeRo, festejoDeOrne, festejoDeLu, festejoDeNacho, festejoDePetru] 

-- ! Propiedades de las listas
-- Las listas son una forma de representar una coleccion de elementos, en este caso, una coleccion de tuplas que representan los cumpleaños.

-- Estas cumplen con concatenacion, que es la propiedad de que se pueden unir dos listas en una sola lista.
-- cumplen con construccion, que es la propiedad de que se pueden construir listas a partir de un elemento y una lista.
-- cumplen con acceso a elementos, que es la propiedad de que se pueden acceder a los elementos de una lista por su posicion.
-- cumplen con longitud, que es la propiedad de que se puede saber la cantidad de elementos que tiene una lista.
-- cumplen con reversa, que es la propiedad de que se puede invertir el orden de los elementos de una lista.
-- cumplen con null, que es la propiedad de que se puede saber si una lista esta vacia o no.

-- Tambien siguen ciertos patrones como si la lista esvacia, lista con cabeza y cola
-- Lista por comprension, que es la propiedad de que se pueden construir listas a partir de una expresion y una lista.

-- Podemos obtener el primer elemento de una lista con head, que es la propiedad de que se puede acceder al primer elemento de una lista.
-- Podemos obtener el resto de los elementos de una lista con tail, que es la propiedad de que se puede acceder al resto de los elementos de una lista.
-- podemos obtener la cantidad de elementos con un map, que es la propiedad de que se puede aplicar una funcion a cada elemento de una lista y obtener una nueva lista con los resultados.

-- & Esta funcion recibe una lista de festejos y devuelve la cantidad total de regalos que hay en todos los festejos.
botinDeRegalos :: [Festejo] -> Int
botinDeRegalos festejos = sum (map cantidadDeRegalos festejos)
-- * botinDeRegalos festejos = sum $ map cantidadDeRegalos festejos

-- & Promedio De regalos
-- ! Funcion inventada
promedioDeRegalos :: [Festejo] -> Int
promedioDeRegalos festejos = div (sum (map cantidadDeRegalos festejos)) (length festejos)
-- * promedioDeRegalos festejos = div (sum $ map cantidadDeRegalos festejos) (length festejos)

-- & Esta funcion nos dice si un cumpleaños es inolvidable o no. Un cumpleaños es inolvidable si es en abril, tiene mas de 400 regalos o la persona que festeja es Gus.
esCumpleaniosInolvidable :: Festejo -> Bool
esCumpleaniosInolvidable festejo = mesDeFestejo festejo == 4 || cantidadDeRegalos festejo > 400 || personaQueFesteja festejo == "Gus"

-- & Esta funcion nos dice si hay algun cumpleaños en diciembre
-- & A diferencia de la otra funcion, aca comparamos la lista completa
-- & Tenemos dos formas de hacerlo, una con any y otra con map y elem
hayCumpleaniosNavidenios :: [Festejo] -> Bool
hayCumpleaniosNavidenios festejos = any (\f -> mesDeFestejo f == 12) festejos
-- * & any verifica si al menos un elemento de la lista cumple con la condicion dada, en este caso si el mes de la fecha es diciembre.
-- * \f -> mesDeFestejo f == 12 es una funcion lambda que recibe un festejo y devuelve True si el mes de la fecha es diciembre

hayCumpleaniosNavidenios2 :: [Festejo] -> Bool
hayCumpleaniosNavidenios2 festejos = 12 `elem` map mesDeFestejo festejos
-- * & map aplica la funcion mesDeFestejo a cada elemento de la lista de festejos y devuelve una lista con los resultados, en este caso una lista de meses.
-- * & elem verifica si un elemento esta en una lista, en este caso si el mes de la fecha es diciembre.

-- La fraseSecreta para poder entrar a los festejos y esta es "La frase es", seguida por la union de todos los nombres de quienes cumplen
-- años pero sacandole las consonantes

fraseSecreta :: [Festejo] -> String
fraseSecreta festejos = "La frase es " ++ concat (map (filter (`elem` "aeiouAEIOU") . personaQueFesteja) festejos)

-- Declaro temáticas según características del festejo
esEducativo :: Festejo -> Bool
esEducativo unFestejo = lugarDeFestejo unFestejo == "Aula"

-- Devolver
tematica :: Festejo -> String
tematica unFestejo
  | esEducativo unFestejo = "Educativo"
  | cantidadDeRegalos unFestejo < 40 = "Ligero"
  | cumpleEn 2 unFestejo = "San Valentín"
  | otherwise = "Sin temática"

-- Implementación de cumpleEn (ya que no estaba definida)
cumpleEn :: Int -> Festejo -> Bool
cumpleEn mes unFestejo = mesDeFestejo unFestejo == mes

{-
-- ! Aca utilizamos data types, pero como anteriormente utilizamos el type festejo, aca no lo podemos utilizar

data Festejo = UnFestejo {
 nombre :: String,
 fecha :: (Int, Int),
 lugar :: String,
 cantRegalos :: Int} -- ~ Record Syntax

 lugarFestejo :: Festejo -> String
 lugarFestejo unFestejo = lugar unFestejo 


data Festejo = UnFestejo String (Int, Int) String Int --~ Positional Syntax

lugarFestejo :: Festejo -> String
lugarFestejo (UnFestejo _ _ lugar _) = lugar 

-}