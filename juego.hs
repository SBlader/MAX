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

newtype Mapa = Mapa [[Celda]]
instance Show Mapa where
    show (Mapa mapa) = unlines (unwords <$> map (fmap show) mapa)

-- Generador de mapa caminable
generarMapaCaminable :: Int -> Int -> Mapa
generarMapaCaminable n m = Mapa $ replicate n (replicate m Camino)

genLava :: (Int,Int) -> [(Int,Int)] -> Int -> StdGen -> [(Int,Int)]
genLava _ posiciones 0 _ = posiciones
genLava (ancho,largo) posiciones n gen =
    let variaciones = [[succ,id],[id,succ,pred],[id,pred]] !! fst (randomR (0,2) gen) in
        genLava (ancho,largo) ([(f x,g y) | f <- variaciones, g <- variaciones] ++ posiciones) (n-1) nextGen
    where
        (x,y,nextGen) = (fst $ randomR (1,ancho-1) nextGen, fst $ randomR (1,largo) gen, snd $ split gen)

-- codigo con el cochino chatgpt
cambiarCelda :: Mapa -> (Int, Int) -> Celda -> Mapa
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
    putStrLn "Coloque un n:"
    nStr <- getLine
    let n = read nStr-1 :: Int
    let mapa = generarMapaCaminable (n+1) (n+1)
    let cantidadPozos = round (1.5 * fromIntegral n) :: Int
    let numeroRandom = 10
    putStrLn "Mapa actualizado: "
    let mapaConLava = foldr (\(x, y) acc -> cambiarCelda acc (x, y) Lava) mapa (genLava (n, n) [] cantidadPozos (mkStdGen numeroRandom))
    loop mapaConLava (0, 0) n n

loop :: Mapa -> (Int, Int) -> Int -> Int -> IO ()
loop mapa (x,y) n m = do
    clearScreen
    print mapa
    mov <- getChar
    let newPos = case mov of
            'w' -> (max 0 (min n (x - 1)), y)
            's' -> (max 0 (min n (x + 1)), y)
            'a' -> (x, max 0 (min m (y - 1)))
            'd' -> (x, max 0 (min m (y + 1)))
            _   -> (x,y)
            -- r -> reiniciar el mapa
    -- if mapa[head newPos][last newPos] == Lava Then Chao ctm   
    let posicionActual = (fst newPos, snd newPos)
    if obtenerCelda mapa posicionActual == Lava
        then putStrLn "moriste"
    -- else if obtenerCelda mapa posicionActual == Muro
    --         loop newMapa (x,y) n m
    else do
        let preMapa = changeValueMap (x, y) Camino mapa
        let newMapa = changeValueMap (fst newPos, snd newPos) Jugador preMapa
        loop newMapa newPos n m

changeValueMap :: (Int, Int) -> Celda -> Mapa -> Mapa
changeValueMap (r, c) value (Mapa matriz) =
    Mapa (changeValueList r (changeValueList c value (matriz !! r)) matriz)

changeValueList :: Int -> a -> [a] -> [a]
changeValueList 0 value (x:xs) = (value:xs)
changeValueList i value (x:xs) =  x:(changeValueList (i-1) (value) (xs))

obtenerCelda :: Mapa -> (Int, Int) -> Celda
obtenerCelda (Mapa matriz) (x, y) = (matriz !! x) !! y