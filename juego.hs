import System.Process
import System.Random

clearScreen :: IO ()
clearScreen = callCommand "clear"

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


-- Generador de mapa caminable
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

genObstaculos :: (Int,Int) -> [(Int,Int)] -> Int -> StdGen -> [(Int,Int)]
genObstaculos _ posiciones 0 _ = posiciones
genObstaculos (ancho,largo) posiciones n gen =
    genObstaculos (ancho,largo) (chunksObstaculos x y !! fst (randomR (0, (+ (-1)) $ length $ chunksObstaculos undefined undefined ) gen) ++ posiciones) (n-1) nextGen
    where
        (x,y,nextGen) = (fst $ randomR (1,ancho-1) gen, fst $ randomR (1,largo-1) nextGen, nextRandom gen)


-- codigo con el cochino chatgpt
cambiarCelda :: Mapa Celda -> (Int, Int) -> Celda -> Mapa Celda
cambiarCelda (Mapa mapa) (x, y) nuevaCelda =
    let (arriba, resto) = splitAt y mapa in
        if null resto then Mapa mapa
        else if length resto == 1 then Mapa $ arriba ++ [cambiarFila (head resto) x nuevaCelda]
        else Mapa $ arriba ++ [cambiarFila (head resto) x nuevaCelda] ++ tail resto

-- codigo sin chat gpt otra vez
cambiarFila :: [Celda] -> Int -> Celda -> [Celda]
cambiarFila fila x nuevaCelda =
    let (antes, despues) = splitAt x fila in
        if length despues <= 1 then antes ++ [nuevaCelda]
        else antes ++ [nuevaCelda] ++ tail despues


main :: IO ()
main = do
    clearScreen
    putStrLn "___  ___  ___  __   __"
    putStrLn "|  \\/  | / _ \\ \\ \\ / /"
    putStrLn "| .  . |/ /_\\ \\ \\ V /" 
    putStrLn "| |\\/| ||  _  | /   \\" 
    putStrLn "| |  | || | | |/ /^\\ \\"
    putStrLn "\\_|  |_/\\_| |_/\\/   \\/"
    putStrLn "Coloque un n:"
    nStr <- getLine
    let numeroRandom = 42
    let rand = mkStdGen numeroRandom
    let n = read nStr-1 :: Int
    let mapa = generarMapaCaminable (n+1) (n+1)
    let cantidadPozos = round (1.5 * fromIntegral n) :: Int
    let cantidadObstaculos = 3 * round (1.5 * fromIntegral n) :: Int
    putStrLn "Mapa actualizado: "
    let mapaConObstaculos = foldr (\(x, y) acc -> cambiarCelda acc (x, y) Obstaculo) mapa (genObstaculos (n,n) [] cantidadObstaculos rand)
    let mapaConLava = foldr (\(x, y) acc -> cambiarCelda acc (x, y) Lava) mapaConObstaculos (genLava (n, n) [] cantidadPozos (nextRandom rand)) 
    loop mapaConLava (0, 0) n n

nextRandom :: StdGen -> StdGen
nextRandom gen = snd $ split gen

loop :: Mapa Celda -> (Int, Int) -> Int -> Int -> IO ()
loop mapa (x,y) n m = do
    clearScreen
    let playerMapa = cambiarCelda mapa (x,y) Jugador
    print playerMapa
    mov <- getChar
    let newPos = case mov of
            'w' -> (x, max 0 (min m (y - 1)))
            's' -> (x, max 0 (min m (y + 1))) 
            'a' -> (max 0 (min n (x - 1)), y)
            'd' -> (max 0 (min n (x + 1)), y)
            _   -> (x,y)
            -- r -> reiniciar el mapa
    -- if mapa[head newPos][last newPos] == Lava Then Chao ctm   
    if obtenerCelda mapa newPos == Lava
        then do
            clearScreen
            putStrLn "__   _______ _   _  ______ _____ ___________ "
            putStrLn "\\ \\ / /  _  | | | | |  _  \\_   _|  ___|  _  \\"
            putStrLn " \\ V /| | | | | | | | | | | | | | |__ | | | |"
            putStrLn "  \\  / | | | | | | | | | | | | | |  __|| | | |"
            putStrLn "  | | \\ \\_/ / |_| | | |/ / _| |_| |___| |/ /" 
            putStrLn "  \\_/  \\___/ \\___/  |___/  \\___/\\____/|___/  "
    else if obtenerCelda mapa newPos == Obstaculo
            then loop mapa (x,y) n m
    else do
        let newMapa = cambiarCelda mapa (x,y) Camino
        loop newMapa newPos n m

obtenerCelda :: Mapa Celda -> (Int, Int) -> Celda
obtenerCelda (Mapa matriz) (x, y) = (matriz !! y) !! x