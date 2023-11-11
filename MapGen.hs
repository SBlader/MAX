module MapGen (Celda(..), Mapa(..),generarMapaCaminable, chunksLava, genLava, chunksObstaculos, genObstaculos, cambiarCelda, nextRandom) where 

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

    generarMapaCaminable :: Int -> Int -> Mapa Celda
    generarMapaCaminable n m = Mapa $ replicate n (replicate m Camino)

    chunksLava :: [[Int -> Int]]
    chunksLava = [[succ,pred,id],[succ,id],[pred,id]]

    genLava :: (Int,Int) -> [(Int,Int)] -> Int -> StdGen -> [(Int,Int)]
    genLava _ posiciones 0 _ = posiciones
    genLava (ancho,largo) posiciones n gen =
        let variacion = chunksLava !! fst (randomR (0,2) gen) in
            genLava (ancho,largo) ([(f x, g y) | f <- variacion, g <- variacion] ++ posiciones) (n-1) nextGen
        where
            (x,y,nextGen) = (fst $ randomR (1,ancho-1) gen, fst $ randomR (1,largo-1) nextGen, nextRandom gen)

    chunksObstaculos :: (Num b, Eq b) => b -> b -> [[(b, b)]]
    chunksObstaculos x y = [
        [(x+1,y),(x,y),(x-1,y)],
        [(x,y+1),(x,y),(x,y-1)]
        ]

    -- codigo sin chat gpt otra vez
    cambiarFila :: [Celda] -> Int -> Celda -> [Celda]
    cambiarFila fila x nuevaCelda =
        let (antes, despues) = splitAt x fila in
            if length despues <= 1 then antes ++ [nuevaCelda]
            else antes ++ [nuevaCelda] ++ tail despues

    genObstaculos :: (Int,Int) -> [(Int,Int)] -> Int -> StdGen -> [(Int,Int)]
    genObstaculos _ posiciones 0 _ = posiciones
    genObstaculos (ancho,largo) posiciones n gen =
        genObstaculos (ancho,largo) (chunksObstaculos x y !! fst (randomR (0, (+ (-1)) $ length $ chunksObstaculos undefined undefined ) gen) ++ posiciones) (n-1) nextGen
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