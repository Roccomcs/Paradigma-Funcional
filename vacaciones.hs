{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use section" #-}

data Turista = UnTurista {
    nivelDeCansancio :: Int,
    nivelDeStress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
}

data Marea
  = Tranquila
  | Moderada
  | Fuerte
  deriving (Eq, Show)

type Idioma = String
type Excursion = Turista -> Turista

mapCansancio :: (Int -> Int) -> Turista -> Turista
mapCansancio funcion unTurista = unTurista {nivelDeCansancio = funcion . nivelDeCansancio $ unTurista}

mapStress :: (Int -> Int) -> Turista -> Turista
mapStress funcion unTurista = unTurista {nivelDeStress = funcion . nivelDeStress $ unTurista}

mapIdioma :: ([Idioma] -> [Idioma]) -> Turista -> Turista
mapIdioma funcion unTurista = unTurista {idiomas = funcion . idiomas $ unTurista}

viajaSinNadie :: Turista -> Bool
viajaSinNadie unTurista
  |viajaSolo unTurista = True
  |otherwise = False

irALaPlaya :: Excursion
irALaPlaya unTurista
  |viajaSinNadie unTurista = mapCansancio (subtract 5) unTurista
  |otherwise = mapStress (subtract 1) unTurista

apreciarPaisaje :: String -> Excursion
apreciarPaisaje unElemento unTurista = mapStress (subtract . letrasDelElemento $ unElemento) unTurista

letrasDelElemento :: String -> Int
letrasDelElemento unElemento = length unElemento

aprenderIdioma :: Idioma -> Excursion
aprenderIdioma unIdioma = mapIdioma (unIdioma :)

caminar :: Int -> Excursion
caminar tiempo unTurista = aumentarCansancio tiempo . reducirStress tiempo $ unTurista

caminar2 :: Int -> Excursion
caminar2 tiempo unTurista = mapCansancio (+ nivelDeIntensidad tiempo) . mapStress (subtract (nivelDeIntensidad tiempo)) $ unTurista
-- Sin la funcion aumentarCansancio ni reducirStress

aumentarCansancio :: Int -> Turista -> Turista
aumentarCansancio intensidad = mapCansancio (+ intensidad)

reducirStress :: Int -> Turista -> Turista
reducirStress intensidad = mapStress (subtract intensidad)

nivelDeIntensidad :: Int -> Int
nivelDeIntensidad minutos = div minutos 4

paseoEnBarco :: Marea -> Excursion
paseoEnBarco unaMarea unTurista
  |unaMarea == Fuerte = mareaFuerte unTurista
  |unaMarea == Moderada = unTurista
  |unaMarea == Tranquila = mareaTranquila unTurista   

mareaFuerte :: Turista -> Turista
mareaFuerte unTurista = mapCansancio (+ 10) . mapStress (+ 6) $ unTurista

mareaTranquila :: Turista -> Turista
mareaTranquila unTurista =  caminar 10 . apreciarPaisaje "mar" . aprenderIdioma "aleman" $ unTurista  

ana :: Turista
ana = UnTurista 21 0 False ["Español"]

beto :: Turista 
beto = UnTurista 15 15 True ["Aleman"]

cathi :: Turista 
cathi = UnTurista 15 15 True ["Aleman"]

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion unaExcursion unTurista = mapStress (subtract (diezPorciento (unaExcursion unTurista))) (unaExcursion unTurista)
-- hacerExcursion unaExcursion unTurista = mapStress (subtract . diezPorciento $ unaExcursion unTurista) $ unaExcursion unTurista
-- Tenemos 2 veces unaExcursion unTurista, la primera para calcular el diezPorciento
-- La segunda para modificar el campo del mapStree
-- El segundo en teoria es mejor pero gpt me dice que no funca

diezPorciento :: Turista -> Int
diezPorciento unTurista = div (nivelDeStress unTurista) 10 

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2
-- funcion dada

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun funcion unTurista unaExcursion = deltaSegun funcion (hacerExcursion unaExcursion unTurista) unTurista

esEducativa :: Turista -> Excursion -> Bool
esEducativa unTurista unaExcursion
  |cantidadDeIdiomas unTurista /= cantidadDeIdiomas (unaExcursion unTurista)  = True
  |otherwise = False

cantidadDeIdiomas :: Turista -> Int
cantidadDeIdiomas unTurista = length . idiomas $ unTurista

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes unTurista listaDeExcursiones = filter (esDesestresante unTurista) listaDeExcursiones

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante unTurista unaExcursion = cantidadDestress unTurista - cantidadDestress (unaExcursion unTurista) >= 3

cantidadDestress :: Turista -> Int
cantidadDestress unTurista = nivelDeStress unTurista

type Tour = [Excursion]
--lista de funciones que, cada una, transforma un turista.

tourCompleto :: Tour
tourCompleto = [caminar 20 . apreciarPaisaje "cascada" . caminar 40 . irALaPlaya . aprenderIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB unaExcursion = [paseoEnBarco Tranquila, unaExcursion, caminar 120]

-- tourCompleto lo separo por . que lo que hago es componer todo en una funcion
--ladoB lo que hago es crear una unica lista con estos componentes

islaVecina :: Marea -> Tour
islaVecina unaMarea
  |unaMarea == Fuerte = [paseoEnBarco Fuerte, apreciarPaisaje "lago", paseoEnBarco Fuerte] 
  |unaMarea == Moderada = [paseoEnBarco Moderada, irALaPlaya, paseoEnBarco Moderada]
  |unaMarea == Tranquila = [paseoEnBarco Tranquila, irALaPlaya, paseoEnBarco Tranquila]

hacerTour :: Tour -> Turista -> Turista  
hacerTour unTour unTurista = componerTour unTour . mapStress (+ cantidadDeExcursiones unTour) $ unTurista

cantidadDeExcursiones :: Tour -> Int
cantidadDeExcursiones unTour = length unTour

componerTour :: Tour -> Turista -> Turista
componerTour = foldr (.) id

propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista listaDeTours = length (filter (esConvincente turista) listaDeTours) > 0
-- True si existe un tour que sea convincente  

esConvincente :: Turista -> Tour -> Bool
esConvincente turista tour = length (filter (dejaAcompaniado turista) (excursionesDesestresantes turista tour)) > 0
-- True si existe una excursion desestresante que deje acompañado al turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista excursion = not . viajaSolo . hacerExcursion excursion $ turista
-- True si despues de hacer excursion no viaja solo

efectividadTour :: [Turista] -> Tour -> Int
efectividadTour listaDeTuristas unTour = sum . map (espiritualidad unTour) $ turistasConvencidos listaDeTuristas unTour

turistasConvencidos :: [Turista] -> Tour -> [Turista]
turistasConvencidos listaDeTuristas unTour = filter (flip turistaConvencido unTour) listaDeTuristas

turistaConvencido :: Turista -> Tour -> Bool
turistaConvencido unTurista unTour = esConvincente unTurista unTour

espiritualidad :: Tour -> Turista ->  Int
espiritualidad unTour unTurista = perdidaDeCansancio unTour unTurista + perdidaDeStress unTour unTurista

perdidaDeCansancio :: Tour -> Turista -> Int
perdidaDeCansancio unTour unTurista = nivelDeCansancio unTurista - nivelDeCansancio (hacerTour unTour unTurista)

perdidaDeStress :: Tour -> Turista -> Int
perdidaDeStress unTour unTurista = nivelDeStress unTurista - nivelDeStress (hacerTour unTour unTurista)

infinitasPlayas :: [Excursion]
infinitasPlayas = map (apreciarPaisaje . ("Playa " ++) . show) [1 .. ]

tourInfinito :: Tour
tourInfinito = infinitasPlayas

{-
4) b. Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
4) C. No se puede saber la efectividad del Tour ya que tendria que sumar una lista infinita lo cual es imposible
-}