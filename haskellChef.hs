{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
data Participante = UnParticipante{
    nombre :: String,
    trucosDeCocina :: [Truco],
    platoEstrella :: Plato
}

data Plato = UnPlato{
    dificultad :: Int,
    componentes :: [Componentes]
}

type Truco = Plato -> Plato
type Componentes = (Ingredientes, Peso)
type Ingredientes = String
type Peso = Int

ingredientes :: Componentes -> String
ingredientes = fst

pesoComida :: Componentes -> Int
pesoComida = snd

mapComponentes :: ([Componentes] -> [Componentes]) -> Plato -> Plato
mapComponentes funcion unPlato = unPlato {componentes = funcion . componentes $ unPlato}

mapDificultad :: (Int -> Int) -> Plato -> Plato
mapDificultad funcion unPlato = unPlato {dificultad = funcion . dificultad $ unPlato}

endulzarAlFinal :: Componentes -> Truco
endulzarAlFinal azucarYCantidad unPlato = mapComponentes (++[azucarYCantidad]) unPlato

endulzarAlPrincipio :: Componentes -> Truco
endulzarAlPrincipio azucarYCantidad unPlato= mapComponentes (azucarYCantidad :) unPlato

salar :: Componentes -> Truco
salar salYCantidad unPlato = mapComponentes (salYCantidad :) unPlato

darSabor :: Componentes -> Componentes -> Truco
darSabor salYCantidad azucarYCantidad unPlato = mapComponentes (++[salYCantidad ,azucarYCantidad]) unPlato

duplicarPorcion :: Truco
duplicarPorcion unPlato = mapComponentes (map  duplicarComponente) unPlato
-- map $ duplicarComponente toma una lista de componentes y devuelve la lista con la funcion aplicada
-- mapComponentes espera una funcion que transforme la lista de componentes

duplicarComponente :: Componentes -> Componentes
duplicarComponente (ingrediente, cantidad) = (ingrediente, cantidad *2)

simplificar ::  Truco 
simplificar  unPlato
  |esComplejo unPlato = modificarDificultad 5 . mapComponentes sacarComponentesLivianos $ unPlato
  |otherwise = unPlato

modificarDificultad :: Int -> Plato -> Plato
modificarDificultad valor unPlato = mapDificultad (const valor) unPlato

cantidadDeComponentes :: Plato -> Int
cantidadDeComponentes unPlato = length . componentes $ unPlato 

esComplejo :: Plato -> Bool
esComplejo unPlato = cantidadDeComponentes unPlato > 5 && dificultad unPlato > 7

sacarComponentesLivianos :: [Componentes] -> [Componentes]
sacarComponentesLivianos = filter esPesado

esPesado :: Componentes -> Bool
esPesado unComponente = pesoComida unComponente > 10

esVegano :: [Componentes] -> Bool
esVegano listaDeComponentes = not . any tieneAnimales $ listaDeComponentes

tieneAnimales :: Componentes -> Bool
tieneAnimales unComponente = estaProhibido ["carne", "huevos", "alimentosLacteos"] unComponente

esSinTacc :: [Componentes] -> Bool
esSinTacc listaDeComponentes= not . any tieneHarina $ listaDeComponentes

tieneHarina :: Componentes -> Bool
tieneHarina unComponente = estaProhibido ["Harina"] unComponente

ningunComponente :: (Componentes -> Bool) -> [Componentes] -> Bool
ningunComponente funcion  = not . any funcion 

estaProhibido :: [String] -> Componentes -> Bool 
estaProhibido prohibidos unComponente = elem (ingredientes unComponente)  prohibidos

noAptoHipertension :: Componentes -> Bool
noAptoHipertension unComponente = ingredientes unComponente == "sal" && pesoComida unComponente > 2

noAptoHipertensionPlato :: [Componentes] -> Bool
noAptoHipertensionPlato listaDeComponentes= any noAptoHipertension listaDeComponentes

pepe :: Participante
pepe = UnParticipante "pepeRonccino" [darSabor ("sal",2) ("azucar",5), simplificar, duplicarPorcion] platoComplejo

platoComplejo :: Plato
platoComplejo = UnPlato 9 [("carne", 50),("huevos", 20), ("sal", 3), ("harina", 15), ("alimentosLacteos", 10)]

cocinar :: Participante -> Plato 
cocinar unParticipante = aplicarTrucos (trucosDeCocina unParticipante) $ platoEstrella unParticipante

aplicarTrucos :: [Truco] -> Plato -> Plato
aplicarTrucos = foldr (.) id
-- foldr (.) id compone todas las funciones de la lista
-- Si la lista está vacía, devuelve el plato tal cual (id).

esMejorQue :: Plato -> Plato -> Bool
esMejorQue primerPlato segundoPlato = esMasDificil primerPlato segundoPlato && esMasLiviano primerPlato segundoPlato

esMasDificil :: Plato -> Plato -> Bool
esMasDificil primerPlato segundoPlato = dificultad primerPlato > dificultad segundoPlato

esMasLiviano :: Plato -> Plato -> Bool
esMasLiviano primerPlato segundoPlato = sumarPesos primerPlato < sumarPesos segundoPlato

sumarPesos :: Plato -> Int
sumarPesos unPlato = sum . map pesoComida $ componentes unPlato

participanteEstrella :: [Participante] -> Participante
participanteEstrella listaDeParticipantes = foldr1 mejorParticipante listaDeParticipantes
--Usa foldr si tu lista puede estar vacía o necesitas un valor inicial.
--Usa foldr1 solo si tu lista siempre tiene al menos un elemento y no necesitas valor inicial

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
  |esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
  |otherwise = otroParticipante

platinum :: Plato 
platinum = UnPlato 10 componentesInfinitos

componentesInfinitos :: [Componentes]
componentesInfinitos = zip nombresInfinitos [1..]

nombresInfinitos :: [String]
nombresInfinitos = map (("Ingrediente " ++) . show) [1..]

{- componentesInfinitos :: [Componentes]
componentesInfinitos = map (\unNumero -> ("Ingrediente " ++ show unNumero, unNumero)) [1..] -}
--Otra opcion

{-
Endulzar al principio y salar, como están al principio de la lista, por lazy evaluation los podemos terminar y mostrar.
Los que se agregan al final como endualzarAlFinal o darSabor no termina nunca por que haskell recorre toda la lista
DuplicarPorcion la lista la duplica pero sigue siendo infinite, no termina nunca
Simplificar no termina de evaluar ya que no para de sacar los componentes de la lista que quiere sacar los < a 10 gramos

Solo esComplejo ya que busca si tiene mas de 5 componentes y la dificultad mayor a 7

La dificultad es mas alta o igual a cualquier otro plato, pero al tener infinitos componentes, no vamos a poder saber si esMasLiviano,
por lo tanto no se puede saber por la lista infinita
-}