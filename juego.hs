import System.Process ( callCommand )
import System.Random ( mkStdGen, StdGen)
import MapGen
import System.Environment ( getArgs )
import System.IO ( hSetBuffering, stdin, BufferMode(NoBuffering) ) 

-- Estructura para recibir la entrada
data Action k = Action {getMov::(Int,Int), getKey::k}

-- Main inicializa los argumentos y el mapa
main :: IO ()
main = do
    -- Desactivar el buffering de entrada (getchar Instantaneo)
    hSetBuffering stdin NoBuffering
    -- Leer los argumentos
    args <- getArgs
    if length args /= 2 then 
        putStrLn "Debes ingresar los argumentos (n::Tamano mapa) y (r::Int generador)"
    else do
        clearScreen
        print args
        -- Leer parametros
        let rand = mkStdGen (read $ last args :: Int) -- Generador con el numero random
        let n = (read $ args !! 0 :: Int) -- Largo mapa
        let cantidadPozos = round (1.5 * fromIntegral n) :: Int
        let cantidadObstaculos = 3 * cantidadPozos :: Int
        -- Ahora se genera mapa vacio, luego se le colocan obstaculos y por ultimo lava
        let mapa = newMapa n cantidadPozos cantidadObstaculos rand
        loop mapa (0, 0) (n) n cantidadPozos cantidadObstaculos rand -- Iniciar Loop


-- Funciones auxiliares
-- Gameloop: Dibujar, Esperar movimiento, Cambiar estado y repetir
loop :: Mapa Celda -> (Int, Int) -> Int -> Int -> Int -> Int -> StdGen -> IO ()
loop mapa (x,y) n m cantidadPozos cantidadLava rand= do
    clearScreen
    print (cambiarCelda mapa (x,y) Jugador)
    -- Iniciar acciones segun el movimiento
    key <- getChar
    let (action, newCelda) = (getAction (x,y) n m key, obtenerCelda mapa $ getMov action) in 
        if (getKey action == 'r') then loop (newMapa n cantidadPozos cantidadLava (nextRandom rand)) (x,y) n m cantidadPozos cantidadLava (nextRandom rand)
        else if newCelda == Lava then deadMessage -- Si vas a caminar en lava        
        else if newCelda == Obstaculo then loop mapa (x,y) n m cantidadPozos cantidadLava rand -- Si vas a caminar en un obstaculo
        else loop (cambiarCelda mapa (x,y) Camino) (getMov action) n m cantidadPozos cantidadLava rand-- Si vas a caminar sobre un suelo caminable

-- Funciones auxiliares
getAction :: (Int, Int) -> Int -> Int -> Char -> Action Char
getAction (x,y) n m key 
    | key == 'w' = Action (x, max 0 (min m (y - 1))) 'w'
    | key == 'a' = Action (max 0 (min n (x - 1)), y) 'a'
    | key == 's' =  Action (x, max 0 (min (m-1) (y + 1)))  's'
    | key == 'd' = Action (max 0 (min (n-1) (x + 1)), y) 'd'
    | otherwise = Action (x,y) key

-- Obtiene el contenido de una celda del mapa
obtenerCelda :: Mapa Celda -> (Int, Int) -> Celda
obtenerCelda (Mapa matriz) (x, y) = (matriz !! y) !! x

-- Limpia la pantalla
clearScreen :: IO ()
clearScreen = callCommand "clear"

-- Devuelve el mensaje de muerte
deadMessage :: IO ()
deadMessage = do
    clearScreen
    putStrLn "@@@ @@@   @@@@@@   @@@  @@@     @@@@@@@   @@@  @@@@@@@@  @@@@@@@   @@@  @@@  "
    putStrLn "@@@ @@@  @@@@@@@@  @@@  @@@     @@@@@@@@  @@@  @@@@@@@@  @@@@@@@@  @@@  @@@  "
    putStrLn "@@! !@@  @@!  @@@  @@!  @@@     @@!  @@@  @@!  @@!       @@!  @@@  @@!  @@!"
    putStrLn "!@! @!!  !@!  @!@  !@!  @!@     !@!  @!@  !@!  !@!       !@!  @!@  !@   !@   "
    putStrLn " !@!@!   @!@  !@!  @!@  !@!     @!@  !@!  !!@  @!!!:!    @!@  !@!  @!@  @!@  "
    putStrLn "  @!!!   !@!  !!!  !@!  !!!     !@!  !!!  !!!  !!!!!:    !@!  !!!  !!!  !!!  "
    putStrLn "  !!:    !!:  !!!  !!:  !!!     !!:  !!!  !!:  !!:       !!:  !!!"            
    putStrLn "  :!:    :!:  !:!  :!:  !:!     :!:  !:!  :!:  :!:       :!:  !:!  :!:  :!:"  
    putStrLn "  ::     ::::: ::  ::::: ::      :::: ::   ::   :: ::::   :::: ::   ::   ::" 
    putStrLn "   :      : :  :    : :  :      :: :  :   :    : :: ::   :: :  :   :::  :::"

newMapa:: Int -> Int -> Int -> StdGen -> Mapa Celda
newMapa n cantidadPozos cantidadObstaculos rand = Mapa $ generarMapa (n) (n) (0,0) (genChunk (n, n) [] cantidadPozos (nextRandom rand) chunksLava) (genChunk (n, n) [] cantidadObstaculos rand chunksObstaculos)
