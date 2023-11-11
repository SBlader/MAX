import System.Process
import System.Random
import MapGen
import System.Environment

clearScreen :: IO ()
clearScreen = callCommand "clear"

main :: IO ()
main = do
    args <- getArgs
    clearScreen
    putStrLn "___  ___  ___  __   __"
    putStrLn "|  \\/  | / _ \\ \\ \\ / /"
    putStrLn "| .  . |/ /_\\ \\ \\ V /" 
    putStrLn "| |\\/| ||  _  | /   \\" 
    putStrLn "| |  | || | | |/ /^\\ \\"
    putStrLn "\\_|  |_/\\_| |_/\\/   \\/"
    putStrLn "Coloque un n:"
    let nStr = head args
    let numeroRandom = read (args !! 1) :: Int
    let rand = mkStdGen numeroRandom
    let n = (+(-1)) $ (read nStr :: Int)
    let cantidadPozos = round (1.5 * fromIntegral n) :: Int
    let cantidadObstaculos = 3 * round (1.5 * fromIntegral n) :: Int
    putStrLn "Mapa actualizado: "
    let mapa = generarMapaCaminable (n+1) (n+1)
    let mapaConObstaculos = foldr (\(x, y) acc -> cambiarCelda acc (x, y) Obstaculo) mapa (genChunk (n,n) [] cantidadObstaculos rand chunksObstaculos)
    let mapaConLava = foldr (\(x, y) acc -> cambiarCelda acc (x, y) Lava) mapaConObstaculos (genChunk (n, n) [] cantidadPozos (nextRandom rand) chunksLava) 
    loop mapaConLava (0, 0) n n

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
            putStrLn "@@@ @@@   @@@@@@   @@@  @@@     @@@@@@@   @@@  @@@@@@@@  @@@@@@@   @@@  @@@  "
            putStrLn "@@@ @@@  @@@@@@@@  @@@  @@@     @@@@@@@@  @@@  @@@@@@@@  @@@@@@@@  @@@  @@@  "
            putStrLn "@@! !@@  @@!  @@@  @@!  @@@     @@!  @@@  @@!  @@!       @@!  @@@  @@!  @@!"
            putStrLn "!@! @!!  !@!  @!@  !@!  @!@     !@!  @!@  !@!  !@!       !@!  @!@  !@   !@   "
            putStrLn " !@!@!   @!@  !@!  @!@  !@!     @!@  !@!  !!@  @!!!:!    @!@  !@!  @!@  @!@  "
            putStrLn "  @!!!   !@!  !!!  !@!  !!!     !@!  !!!  !!!  !!!!!:    !@!  !!!  !!!  !!!  "
            putStrLn "  !!:    !!:  !!!  !!:  !!!     !!:  !!!  !!:  !!:       !!:  !!!"            
            putStrLn "  :!:    :!:  !:!  :!:  !:!     :!:  !:!  :!:  :!:       :!:  !:!  :!:  :!:"  
            putStrLn "  ::    ::::: ::  ::::: ::      :::: ::   ::   :: ::::   :::: ::   ::   ::" 
            putStrLn "  :      : :  :    : :  :      :: :  :   :    : :: ::   :: :  :   :::  :::"
    else if obtenerCelda mapa newPos == Obstaculo
            then loop mapa (x,y) n m
    else do
        let newMapa = cambiarCelda mapa (x,y) Camino
        loop newMapa newPos n m

obtenerCelda :: Mapa Celda -> (Int, Int) -> Celda
obtenerCelda (Mapa matriz) (x, y) = (matriz !! y) !! x