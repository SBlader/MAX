import System.Process ( callCommand )
import System.Random ( mkStdGen, StdGen, randomRs )
import MapGen
import System.Environment ( getArgs )
import System.IO ( hSetBuffering, stdin, BufferMode(NoBuffering) ) 
import GHC.Float.RealFracMethods

-- Estructura para recibir la entrada
data Action k = Action {getMov::(Int,Int), getKey::k}

data Score x = Score x | Nada
instance (Show a) => Show (Score a) where
    show (Score x) = "Tienes " ++ show x ++ " runas de poder!\n"
    show (Nada) = "No tienes ninguna runa de poder aun...\n"

instance Functor Score where
    fmap _ Nada = Nada
    fmap f (Score x) = Score (f x)

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
        let (n, rand) = ((read $ args !! 0 :: Int), mkStdGen (read $ last args :: Int)) -- Generador con el numero random
        let (posTesoro, posRunas) = (anyPos rand n, foldl (\ acc x -> anyPos x n : acc) [] $ take ((*2) . roundFloatInt . log . fromIntegral $ (n)) $ fmap mkStdGen (randomRs (1,1000) rand))
        let (cantidadPozos, cantidadObstaculos) = (round (fromIntegral n * fromIntegral n * 0.06) :: Int, 3 * cantidadPozos :: Int)
        -- Ahora se genera mapa vacio, luego se le colocan obstaculos y por ultimo lava
        let mapa = makeMapa n posRunas posTesoro cantidadPozos cantidadObstaculos rand
        loop mapa (0, 0) posRunas posTesoro n n cantidadPozos cantidadObstaculos (strongRandom 100 rand) -- Iniciar Loop
    

-- Gameloop: Dibujar, Esperar movimiento, Cambiar estado y repetir
loop :: Mapa Celda -> (Int, Int) -> [(Int,Int)] -> (Int, Int) -> Int -> Int -> Int -> Int -> StdGen -> IO ()
loop mapa (x,y) posRunas tesoro n m cantidadPozos cantidadLava rand = do
    clearScreen
    print (cambiarCelda mapa (x,y) Jugador)
    -- Iniciar acciones segun el movimiento
    key <- getChar
    let (action, newCelda) = (getAction (x,y) n m key, obtenerCelda mapa $ getMov action) in 
        if (getKey action == 'r') then loop (makeMapa n posRunas tesoro cantidadPozos cantidadLava (strongRandom 50 rand)) (x,y) posRunas tesoro n m cantidadPozos cantidadLava (strongRandom 100 rand)
        else if (getKey action == 'h') then do
            help                                                                        --Ve los controles
            loop mapa (x,y) posRunas tesoro n m cantidadPozos cantidadLava rand
        else if newCelda == Lava then deadMessage -- Si vas a caminar en lava        
        else if newCelda == Obstaculo then loop mapa (x,y) posRunas tesoro n m cantidadPozos cantidadLava rand -- Si vas a caminar en un obstaculo
        else if newCelda == Tesoro then winMessage -- Si vas a caminar a la celda del tesoro
        else loop (cambiarCelda mapa (x,y) Camino) (getMov action) posRunas tesoro n m cantidadPozos cantidadLava rand-- Si vas a caminar sobre un suelo caminable

-- FUNCIONES AUXILIARES

--Recive la accion realizada por el usuario y actualiza las coordenadas del jugador correspondientemente.
getAction :: (Int, Int) -> Int -> Int -> Char -> Action Char
getAction (x,y) n m key 
    | key == 'w' = Action (x, max 0 (min m (y - 1))) 'w'
    | key == 'a' = Action (max 0 (min n (x - 1)), y) 'a'
    | key == 's' =  Action (x, max 0 (min (m-1) (y + 1)))  's'
    | key == 'd' = Action (max 0 (min (n-1) (x + 1)), y) 'd'
    | otherwise = Action (x,y) key

anyPos :: StdGen -> Int -> (Int, Int)
anyPos gen n = ((\[x,y]->(x,y)) (take 2 $ randomRs (0, (n - 1)) gen))

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

winMessage :: IO ()
winMessage = do
    clearScreen
    putStrLn "******************************************************************************* \n\
\          |                   |                  |                     |\n\
\ _________|________________.=\"\"_;=.______________|_____________________|_______\n\
\|                   |  ,-\"_,=\"\"     `\"=.|                  |\n\
\|___________________|__\"=._o`\"-._        `\"=.______________|___________________\n\
\          |                `\"=._o`\"=._      _`\"=._                     |\n\
\ _________|_____________________:=._o \"=._.\"_.-=\"'\"=.__________________|_______\n\
\|                   |    __.--\" , ; `\"=._o.\" ,-\"\"\"-._ \".   |\n\
\|___________________|_._\"  ,. .` ` `` ,  `\"-._\"-._   \". '__|___________________\n\
\          |           |o`\"=._` , \"` `; .\". ,  \"-._\"-._; ;              |\n\
\ _________|___________| ;`-.o`\"=._; .\" ` '`.\"\\` . \"-._ /_______________|_______\n\
\|                   | |o;    `\"-.o`\"=._``  '` \" ,__.--o;   |\n\
\|___________________|_| ;     (#) `-.o `\"=.`_.--\"_o.-; ;___|___________________\n\
\____/______/______/___|o;._    \"      `\".o|o_.--\"    ;o;____/______/______/____\n\
\/______/______/______/_\"=._o--._        ; | ;        ; ;/______/______/______/_\n\
\____/______/______/______/__\"=._o--._   ;o|o;     _._;o;____/______/______/____\n\
\/______/______/______/______/____\"=._o._; | ;_.--\"o.--\"_/______/______/______/_\n\
\____/______/______/______/______/_____\"=.o|o_.--\"\"___/______/______/______/____\n\
\/______/______/______/______/______/______/______/______/______/______/[YOU WIN!]\n\
\*******************************************************************************"

-- funcion auxiliar para simplificar la generacion del mapa en otras funciones.
makeMapa :: Int -> [(Int, Int)] -> (Int, Int) -> Int -> Int -> StdGen -> Mapa Celda
makeMapa n runas tesoro cantidadPozos cantidadObstaculos rand
    | n > 10 = Mapa $ generarMapa n n (0,0) tesoro (genChunk (n, n) [] cantidadPozos (nextRandom rand) chunksLava) (genChunk (n, n) [] cantidadObstaculos rand chunksObstaculos)
    | n <= 10 = Mapa $ generarMapa n n (0,0) tesoro (genChunk (n, n) [] cantidadPozos (nextRandom rand) (chunksLavaSmall rand)) (genChunk (n, n) [] cantidadObstaculos rand chunksObstaculos)
-- itera multiples veces el generador para que se realicen grandes cambios al reiniciar el mapa.
strongRandom :: Int -> StdGen -> StdGen
strongRandom strength generator = foldl (\ gen _ -> nextRandom gen ) generator [1..strength]

tutorial :: IO()
tutorial = do
    clearScreen
    putStrLn "___  ___  ___  __   __"
    putStrLn "|  \\/  | / _ \\ \\ \\ / /"
    putStrLn "| .  . |/ /_\\ \\ \\ V /" 
    putStrLn "| |\\/| ||  _  | /   \\" 
    putStrLn "| |  | || | | |/ /^\\ \\"
    putStrLn "\\_|  |_/\\_| |_/\\/   \\/"

    putStrLn "Bienvenido a Mini Adventure X. \n\
    \Estás a punto de entrar a una mazmorra llena de peligros y un abundante tesoro\n\
    \encontraras pozos de lava ($) y murallas (L) que bloquearán el camino hacia tu merecido premio\n\
    \para llegar allá puedes moverte usando las teclas W-A-S-D.\n\
    \\nPara ayudarte en tu camino tienes un super poder, puedes hacer que la mazmorra cambie su forma de manera aleatoria \n\
    \pero sin moverte ni tú, ni el tesoro, para usar este poder presiona la tecla R.\n\
    \\nNo dudes en pedir ayuda si lo necesitas, siempre puedes ver los controles con la tecla H \n\
    \\nAhora ve, valiente héroe, y reclama tu lugar entre las leyendas.\n\
    \\n\n Presiona Enter para continuar."

    getLine
    clearScreen

help :: IO()
help = do
    clearScreen
    putStrLn "Arriba: W\n\
    \Abajo: S\n\
    \Izquieda: A\n\
    \Derecha: D\n\
    \Reordenar mazmorra: R\n\
    \\nPresiona cualquier tecla para continuar..."
    getChar
    clearScreen 