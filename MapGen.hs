module MapGen (Celda(..), Mapa(..),generarMapa, generarFilaMapa, chunksLava, chunksObstaculos, cambiarCelda, nextRandom, genChunk) where 

    import System.Process
    import System.Random

    data Celda = Obstaculo | Camino | Lava | Jugador | Tesoro deriving Eq
    instance Show Celda where
        show Obstaculo = "L"
        show Camino = " "
        show Lava = "$"
        show Jugador = "@"
        show Tesoro = "x"

    newtype Mapa c = Mapa [[c]]
    instance (Show c) => Show (Mapa c) where
        show (Mapa mapa) = unlines (unwords <$> map (fmap show) mapa)

-- Genera el mapa
    generarMapa :: Int -> Int -> (Int,Int) -> [(Int,Int)]  -> [(Int,Int)] -> [[Celda]]
    generarMapa n m (x,y) lav obs
        |m==0 = []
        |otherwise = generarFilaMapa n (x,y) lav obs : generarMapa n (m-1) (x,y+1) lav obs

-- Genera filas para el mapa, si la posicion esta en la lista de posos de lava la celda es lava, si esta en la lista de obstaculos es obstaculo
-- y si no es camino.
    generarFilaMapa:: Int -> (Int,Int) -> [(Int,Int)]  -> [(Int,Int)] -> [Celda]
    generarFilaMapa n (x,y) lav obs
        |n==0 = []
        |elem (x,y) lav == True = Lava : generarFilaMapa (n-1) (x+1,y) lav obs
        |elem (x,y) obs == True = Obstaculo : generarFilaMapa (n-1) (x+1,y) lav obs
        |otherwise = Camino : generarFilaMapa (n-1) (x+1,y) lav obs

-- Genera las posiciones de los pozos de lava que estaran en el mapa
    chunksLava :: Int -> Int -> [[(Int,Int)]] 
    chunksLava x y = [
        [(x+i, y+j) | i<-[-1..1], j<-[-1..1]],
        [(x+i,y+j) | i<-[-1,0],j<-[-1,0]], 
        [(x+i, y+j) | i<-[0,1], j<-[0,1]],
        [(x+1,y),(x,y),(x-1,y),(x,y+1),(x,y-1)]
        ]

-- Genera los obstaculos que estaran en el mapa
    chunksObstaculos :: Int -> Int -> [[(Int,Int)]]
    chunksObstaculos x y = [
        [(x+1,y),(x,y),(x-1,y)],
        [(x,y+1),(x,y),(x,y-1)]
        ]

    cambiarFila :: [Celda] -> Int -> Celda -> [Celda]
    cambiarFila fila x nuevaCelda =
        let (antes, despues) = splitAt x fila in
            if length despues <= 1 then antes ++ [nuevaCelda]
            else antes ++ [nuevaCelda] ++ tail despues

    genChunk :: (Int,Int) -> [(Int,Int)] -> Int -> StdGen -> (Int -> Int -> [[(Int,Int)]]) -> [(Int,Int)]
    genChunk _ posiciones 0 _ _ = posiciones
    genChunk (ancho,largo) posiciones n gen chunksGen =
        genChunk (ancho,largo) (chunksGen x y !! fst (randomR (0, (+ (-1)) $ length $ chunksGen undefined undefined ) gen) ++ posiciones) (n-1) nextGen chunksGen
        where
            (x,y,nextGen) = (fst $ randomR (1,ancho-1) gen, fst $ randomR (1,largo-1) nextGen, nextRandom gen)

    cambiarCelda :: Mapa Celda -> (Int, Int) -> Celda -> Mapa Celda
    cambiarCelda (Mapa mapa) (x, y) nuevaCelda =
        let (arriba, resto) = splitAt y mapa in
            if null resto then Mapa mapa
            else if length resto == 1 then Mapa $ arriba ++ [cambiarFila (head resto) x nuevaCelda]
            else Mapa $ arriba ++ [cambiarFila (head resto) x nuevaCelda] ++ tail resto

    nextRandom :: StdGen -> StdGen
    nextRandom gen = snd $ split gen