{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Hoist not" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant $" #-}

data Auto = UnAuto{
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show, Eq)

type Carrera = [Auto]

estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto otroAuto = sonDistintos unAuto otroAuto && distanciaEntre unAuto otroAuto < 10

distanciaEntre :: Auto -> Auto -> Int
distanciaEntre unAuto otroAuto = abs (distancia unAuto - distancia otroAuto)

sonDistintos :: Auto -> Auto -> Bool
sonDistintos unAuto otroAuto = unAuto /= otroAuto

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto unaCarrera = all (not . estaCerca unAuto) unaCarrera && estaGanando unAuto unaCarrera  

estaGanando :: Auto -> Carrera -> Bool
estaGanando unAuto unaCarrera = all (vaAdelante unAuto) . filter (sonDistintos unAuto) $ unaCarrera

vaAdelante :: Auto -> Auto -> Bool
vaAdelante unAuto otroAuto = distancia unAuto >= distancia otroAuto

puesto :: Auto -> Carrera -> Int
puesto unAuto unaCarrera = length . filter (flip vaAdelante unAuto) $ unaCarrera
-- Tomamos vaAdelante junto a un auto y otro auto de la listaDeCarrera, invertimos el orden
-- En el que se agarran los parametros con el flip para analizar primero el de la lista y despues el seleccionado
-- filtramos los autos que van adelante y eliminamos los que estan atras y con el length vemos cuantos tenemos en
-- total en la lista y le sumamos 1

correr ::  Int -> Auto -> Auto
correr unTiempo unAuto = mapDistancia (+ (unTiempo * velocidad unAuto)) unAuto 

mapDistancia :: (Int -> Int) -> Auto -> Auto
mapDistancia funcion unAuto = unAuto {distancia = funcion . distancia $ unAuto}

alterarLaVelocidad :: (Int -> Int) -> Auto -> Auto
alterarLaVelocidad funcion unAuto = unAuto {velocidad = max 0 . funcion . velocidad $ unAuto}

bajarLaVelocidad :: Int -> Auto -> Auto
bajarLaVelocidad unValor unAuto = alterarLaVelocidad (subtract unValor) unAuto

type PowerUps = Auto -> Carrera -> Carrera

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: PowerUps
terremoto unAuto unaCarrera = afectarALosQueCumplen (estaCerca unAuto) (bajarLaVelocidad 50) $ unaCarrera

miguelitos :: Int -> PowerUps
miguelitos bajada unAuto unaCarrera = afectarALosQueCumplen (vaAdelante unAuto) (bajarLaVelocidad bajada) $ unaCarrera

jetpack :: Int -> PowerUps
jetpack duracion unAuto unaCarrera = afectarALosQueCumplen (==unAuto) (efectoJetpack duracion) $ unaCarrera

efectoJetpack :: Int -> Auto -> Auto
efectoJetpack duracion unAuto = restaurarVelocidad unAuto . correr duracion . alterarLaVelocidad (*2) $ unAuto

restaurarVelocidad :: Auto -> Auto -> Auto
restaurarVelocidad unAuto unAutoModificado = unAutoModificado {velocidad = velocidad unAuto}

type Color = String
type Evento = Carrera -> Carrera

simularCarrera :: Carrera -> [Evento] -> [(Int, Color)]
simularCarrera unaCarrera listaDeEventos = (obtenerTablaDePosiciones . producirEventos listaDeEventos) unaCarrera

entradaDeTabla :: Carrera -> Auto -> (Int, Color)
entradaDeTabla unaCarrera unAuto = (puesto unAuto unaCarrera, color unAuto)  

obtenerTablaDePosiciones :: Carrera -> [(Int, Color)]
obtenerTablaDePosiciones unaCarrera = map (entradaDeTabla unaCarrera) unaCarrera

producirEventos :: [Evento] -> Evento
producirEventos = foldr (.) id

correnTodos :: Int -> Evento
correnTodos duracion unaCarrera = map (correr duracion) unaCarrera

usaPowerUp :: PowerUps -> Color -> Carrera -> Carrera
usaPowerUp powerUp colorBuscado carrera = powerUp (encontrarPorColor colorBuscado carrera) carrera

encontrarPorColor :: Color -> Carrera -> Auto
encontrarPorColor colorBuscado = head . filter (tieneColor colorBuscado)

tieneColor :: Color -> Auto -> Bool
tieneColor colorBuscado auto = color auto == colorBuscado

autoRojo :: Auto
autoRojo   = UnAuto "Rojo"   120 0
autoBlanco :: Auto
autoBlanco = UnAuto "Blanco" 120 0
autoAzul :: Auto
autoAzul   = UnAuto "Azul"   120 0
autoNegro :: Auto
autoNegro  = UnAuto "Negro"  120 0

carreraInicial :: Carrera
carreraInicial = [autoRojo, autoBlanco, autoAzul, autoNegro]

eventos :: [Evento]
eventos =
  [ correnTodos 30
  , usaPowerUp (jetpack 3) "Azul"
  , usaPowerUp terremoto "Blanco"
  , correnTodos 40
  , usaPowerUp (miguelitos 20) "Blanco"
  , usaPowerUp (jetpack 6) "Negro"
  , correnTodos 10
  ]

  {-
  a. Si se puede agregar
  b. No van a poder terminar de evaluarse nunca ya que comparan un auto con otro auto, al haber infinitos autos nunca cortaria
  -}