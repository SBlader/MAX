module MapGen where 

    import System.Process
    import System.Random


    {- ESTRUCTURAS -}
    -- Celda es un tipo creado para definir los estados que puede tener una celda en el juego.
    data Celda = Obstaculo | Camino | Lava | Jugador | Tesoro | Runa deriving Eq
    instance Show Celda where
        show Obstaculo = "L"
        show Camino = " "
        show Lava = "$"
        show Jugador = "@"
        show Tesoro = "x"
        show Runa = "λ"

    -- Mapa es un tipo creado para poder imprimir la lista de listas ademas de hacer mas entendible el codigo.
    newtype Mapa c = Mapa [[c]]
    instance (Show c) => Show (Mapa c) where
        show (Mapa mapa) = unlines (unwords <$> map (fmap show) mapa)

    -- Estructura para recibir la entrada
    data Action k = Action {getMov::(Int,Int), getKey::k}

    -- Estructura para guardar el multiplicador
    data Score x = Score x | Nada
    instance (Show a) => Show (Score a) where
        show (Score x) = "Ganaste " ++ show x ++ " puntos, bien hecho!"
        show (Nada) = "No tienes puntos extra..."
    -- Functor permite modificar el multiplicador
    instance Functor Score where
        fmap _ Nada = Nada
        fmap f (Score x) = Score (f x)

    {- FUNCIONES DE GENERACION Y MAPA -}
    -- Genera el mapa con todos los obstaculos y pozos de lava.
    generarMapa :: Int -> Int -> (Int,Int) -> [(Int,Int)] -> (Int,Int) -> [(Int,Int)]  -> [(Int,Int)] -> [[Celda]]
    generarMapa n m (x,y) runas tesoro lav obs
        | m == 0 = []
        | otherwise = generarFilaMapa n (x,y) runas tesoro lav obs : generarMapa n (m-1) (x,y+1) runas tesoro lav obs

    -- Genera filas para el mapa, si la posicion esta en la lista de posos de lava la celda es lava, y asi respectivamente.
    generarFilaMapa :: Int -> (Int,Int) -> [(Int, Int)] -> (Int,Int) -> [(Int,Int)]  -> [(Int,Int)] -> [Celda]
    generarFilaMapa n (x,y) runas tesoro lav obs
        | n == 0 = []
        | (x, y) == tesoro = Tesoro : generarFilaMapa (n-1) (x+1,y) runas tesoro lav obs
        | elem (x, y) runas = Runa : generarFilaMapa (n-1) (x+1,y) runas tesoro lav obs
        | elem (x, y) lav == True = Lava : generarFilaMapa (n-1) (x+1,y) runas tesoro lav obs
        | elem (x, y) obs == True = Obstaculo : generarFilaMapa (n-1) (x+1,y) runas tesoro lav obs
        | otherwise = Camino : generarFilaMapa (n-1) (x+1,y) runas tesoro lav obs

    -- Genera la forma de la lava tendiendo a hacer pozos.
    chunksLava :: Int -> Int -> [[(Int,Int)]] 
    chunksLava x y = [
        [(x+i, y+j) | i<-[-1..1], j<-[-1..1]],
        [(x+i,y+j) | i<-[-1,0],j<-[-1,0]], 
        [(x+i, y+j) | i<-[0,1], j<-[0,1]],
        [(x+1,y),(x,y),(x-1,y),(x,y+1),(x,y-1)]
        ]

    -- Version de generador para mapas pequenos
    chunksLavaSmall :: StdGen -> Int -> Int -> [[(Int,Int)]] 
    chunksLavaSmall rand x y = [
        [(x+1,y),(x,y),(x-1,y),(x,y+1),(x,y-1)],
        [(\(x, y)->(((fst $ randomR (-1, 1) rand)+x), ((fst $ randomR (-1, 1) rand)+y)))(x, y)]
        ]

    -- Genera la forma de los obstaculos tendiendo a hacer murallas.
    chunksObstaculos :: Int -> Int -> [[(Int,Int)]]
    chunksObstaculos x y = [
        [(x+1,y),(x,y),(x-1,y)],
        [(x,y+1),(x,y),(x,y-1)]
        ]

    -- Una funcion de apoyo para cambiarCelda que modifica una fila.
    cambiarFila :: [Celda] -> Int -> Celda -> [Celda]
    cambiarFila fila x nuevaCelda =
        let (antes, despues) = splitAt x fila in
            if length despues <= 1 then antes ++ [nuevaCelda]
            else antes ++ [nuevaCelda] ++ tail despues

    -- Segun una figura, la genera multiples veces como coordenadas en un  espacio nxn.
    genChunk :: (Int,Int) -> [(Int,Int)] -> Int -> StdGen -> (Int -> Int -> [[(Int,Int)]]) -> [(Int,Int)]
    genChunk _ posiciones 0 _ _ = posiciones
    genChunk (ancho,largo) posiciones n gen chunksGen =
        genChunk (ancho,largo) (chunksGen x y !! fst (randomR (0, (+ (-1)) $ length $ chunksGen undefined undefined ) gen) ++ posiciones) (n-1) nextGen chunksGen
        where
            (x,y,nextGen) = (fst $ randomR (1,ancho-1) gen, fst $ randomR (1,largo-1) nextGen, nextRandom gen)

    -- Modifica el mapa cuando se mueve el jugador para que la celda anerior quede como caminable.
    cambiarCelda :: Mapa Celda -> (Int, Int) -> Celda -> Mapa Celda
    cambiarCelda (Mapa mapa) (x, y) nuevaCelda =
        let (arriba, resto) = splitAt y mapa in
            if null resto then Mapa mapa
            else if length resto == 1 then Mapa $ arriba ++ [cambiarFila (head resto) x nuevaCelda]
            else Mapa $ arriba ++ [cambiarFila (head resto) x nuevaCelda] ++ tail resto

    -- Itera al siguiente random del generador.
    nextRandom :: StdGen -> StdGen
    nextRandom gen = snd $ split gen

    -- Genera cualquier posicion
    anyPos :: StdGen -> Int -> (Int, Int)
    anyPos gen n = ((\[x,y] -> (x,y)) (take 2 $ randomRs (0, (n - 1)) gen))

    -- Genera la posicion del tesoro (Hasta que sea distinta de 0,0)
    genTesoro :: StdGen -> Int -> (Int, Int)
    genTesoro gen n = let newPos = anyPos gen n in if (newPos == (0,0)) then genTesoro (nextRandom gen) n else newPos 

    -- Obtiene el contenido de una celda del mapa
    obtenerCelda :: Mapa Celda -> (Int, Int) -> Celda
    obtenerCelda (Mapa matriz) (x, y) = (matriz !! y) !! x

    -- Funcion generadora de mapa, que cambia la generacion segun el tamano del mapa
    makeMapa :: Int -> [(Int, Int)] -> (Int, Int) -> Int -> Int -> StdGen -> Mapa Celda
    makeMapa n runas tesoro cantidadPozos cantidadObstaculos rand
        | n >= 10 = Mapa $ generarMapa n n (0,0) runas tesoro (genChunk (n, n) [] cantidadPozos (nextRandom rand) chunksLava) (genChunk (n, n) [] cantidadObstaculos rand chunksObstaculos)
        | n < 10 = Mapa $ generarMapa n n (0,0) runas tesoro (genChunk (n, n) [] cantidadPozos (nextRandom rand) (chunksLavaSmall rand)) (genChunk (n, n) [] cantidadObstaculos rand chunksObstaculos)

    -- Itera sobre el random para generar mas diferencias
    strongRandom :: Int -> StdGen -> StdGen
    strongRandom strength generator = foldl (\ gen _ -> nextRandom gen ) generator [1..strength]

    {- OTRAS FUNCIONES -}
    -- Revisa si no hay mulitplicador
    isNada :: Score a -> Bool
    isNada Nada = True
    isNada _ = False