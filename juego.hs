import System.Process ( callCommand )
import System.Random ( mkStdGen, StdGen, randomRs )
import MapGen
import System.Environment ( getArgs )
import System.IO ( hSetBuffering, stdin, BufferMode(NoBuffering) ) 
import Data.List ( delete )
import GHC.Float.RealFracMethods ( roundFloatInt )


{- ESTRUCTURAS -}
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


{- FUNCIONAMIENTO -}
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
        -- Leer parametros
        let (n, rand) = ((read $ args !! 0 :: Int), mkStdGen (read $ last args :: Int)) -- Extraer los argumentos
        let (posTesoro, posRunas) = (anyPos rand n, foldl (\ acc x -> anyPos x n : acc) [] $ take ((*2) . roundFloatInt . log . fromIntegral $ (n)) $ fmap mkStdGen (randomRs (1,1000) rand)) -- Generar las posiciones de runas y el tesoro
        let (cantidadPozos, cantidadObstaculos) = (round (fromIntegral n * fromIntegral n * 0.06) :: Int, 3 * cantidadPozos :: Int) -- Generar la cantidad de los obstaculos
        -- Generar el mapa
        let mapa = makeMapa n posRunas posTesoro cantidadPozos cantidadObstaculos rand
        loop Nada mapa (0, 0) posRunas posTesoro n n cantidadPozos cantidadObstaculos (strongRandom 100 rand) -- Iniciar Loop
    
-- Gameloop: Dibujar, Esperar movimiento, Cambiar estado y repetir
loop :: Score Int -> Mapa Celda -> (Int, Int) -> [(Int,Int)] -> (Int, Int) -> Int -> Int -> Int -> Int -> StdGen -> IO ()
loop score mapa (x,y) posRunas tesoro n m cantidadPozos cantidadLava rand = do
    -- Dibujar la pantalla y posicionar al jugador
    clearScreen
    putStrLn $ foldr1 (\ x acc -> x ++ acc ) (replicate (((n-1) `div` 2)) "=-") ++ "MAX" ++ foldr1 (\ x acc -> x ++ acc) (replicate ((n-2) `div` 2) "-=") -- Barra superior
    print $ cambiarCelda mapa (x,y) Jugador -- Colocar al jugador en el mapa
    putStrLn $ replicate (2*n-1) '~' -- Barra inferior
    putStrLn $ if (length posRunas /= 0) then " λ: Quedan " ++ (show . length) posRunas ++ " runas de poder!" else " λ: Tienes todas las runas de poder!" -- Cuantas runas quedan

    -- Iniciar acciones segun el movimiento
    key <- getChar
    let (action, newCelda) = (getAction (x,y) n m key, obtenerCelda mapa $ getMov action) in 
        if (getKey action == 'r') then loop score (makeMapa n posRunas tesoro cantidadPozos cantidadLava (strongRandom 50 rand)) (x,y) posRunas tesoro n m cantidadPozos cantidadLava (strongRandom 100 rand) -- Reintentar
        else if (getKey action == 'h') then do -- Menu ayuda
            help                                                                        --Ve los controles
            loop score mapa (x,y) posRunas tesoro n m cantidadPozos cantidadLava rand
        else if newCelda == Runa then loop ((*2) <$> (if isNada score then Score 1 else score)) mapa (getMov action) (delete (getMov action) posRunas) tesoro n m cantidadPozos cantidadLava rand -- Si llegas a una runa score = score*2
        else if newCelda == Lava then deathMessage -- Si vas a pisar lava     
        else if newCelda == Obstaculo && length posRunas /= 0 then loop score mapa (x,y) posRunas tesoro n m cantidadPozos cantidadLava rand -- Si vas a chocar con un obstaculo
        else if newCelda == Tesoro then winMessage $ (\ x -> round $ (*) (log (fromIntegral n)) (fromIntegral x)) <$> score -- Si llegas al tesoro
        else loop score (cambiarCelda mapa (x,y) Camino) (getMov action) posRunas tesoro n m cantidadPozos cantidadLava rand -- Suelo caminable


{- OTRAS FUNCIONES -}
--Recive la accion realizada por el usuario y actualiza las coordenadas del jugador correspondientemente.
getAction :: (Int, Int) -> Int -> Int -> Char -> Action Char
getAction (x,y) n m key 
    | key == 'w' = Action (x, max 0 (min m (y - 1))) 'w'
    | key == 'a' = Action (max 0 (min n (x - 1)), y) 'a'
    | key == 's' =  Action (x, max 0 (min (m-1) (y + 1))) 's'
    | key == 'd' = Action (max 0 (min (n-1) (x + 1)), y) 'd'
    | otherwise = Action (x,y) key

anyPos :: StdGen -> Int -> (Int, Int)
anyPos gen n = ((\[x,y] -> (x,y)) (take 2 $ randomRs (0, (n - 1)) gen))

-- Obtiene el contenido de una celda del mapa
obtenerCelda :: Mapa Celda -> (Int, Int) -> Celda
obtenerCelda (Mapa matriz) (x, y) = (matriz !! y) !! x

-- Limpia la pantalla
clearScreen :: IO ()
clearScreen = callCommand "clear"

-- Revisa si no hay mulitplicador
isNada :: Score a -> Bool
isNada Nada = True
isNada _ = False

-- Devuelve el mensaje de muerte
deathMessage :: IO ()
deathMessage = do
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

-- Mensaje de victoria
winMessage :: (Show s) => Score s -> IO ()
winMessage score = do
    clearScreen
    putStrLn $ "******************************************************************************* \n\
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
    \|___________________|_| ;     (λ) `-.o `\"=.`_.--\"_o.-; ;___|___________________\n\
    \____/______/______/___|o;._    \"      `\".o|o_.--\"    ;o;____/______/______/____\n\
    \/______/______/______/_\"=._o--._        ; | ;        ; ;/______/______/______/_\n\
    \____/______/______/______/__\"=._o--._   ;o|o;     _._;o;____/______/______/____\n\
    \/______/______/______/______/____\"=._o._; | ;_.--\"o.--\"_/______/______/______/_\n\
    \____/______/______/______/______/_____\"=.o|o_.--\"\"___/______/______/______/____\n\
    \/______/______/______/______/______/______/______/______/______/______/_____/__\n\
    \*******************************************************************************"
    print score
    putStrLn "Haz conseguido el tesoro!"

-- Funcion generadora de mapa, que cambia la generacion segun el tamano del mapa
makeMapa :: Int -> [(Int, Int)] -> (Int, Int) -> Int -> Int -> StdGen -> Mapa Celda
makeMapa n runas tesoro cantidadPozos cantidadObstaculos rand
    | n > 10 = Mapa $ generarMapa n n (0,0) runas tesoro (genChunk (n, n) [] cantidadPozos (nextRandom rand) chunksLava) (genChunk (n, n) [] cantidadObstaculos rand chunksObstaculos)
    | n <= 10 = Mapa $ generarMapa n n (0,0) runas tesoro (genChunk (n, n) [] cantidadPozos (nextRandom rand) (chunksLavaSmall rand)) (genChunk (n, n) [] cantidadObstaculos rand chunksObstaculos)

-- Itera sobre el random para generar mas diferencias
strongRandom :: Int -> StdGen -> StdGen
strongRandom strength generator = foldl (\ gen _ -> nextRandom gen ) generator [1..strength]

-- Pantalla inicial
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
    \\nPara ayudarte en tu camino tienes un super poder, puedes hacer que la mazmorra cambie su forma de manera aleatoria, \n\
    \pero sin moverte ni tú, ni el tesoro, para usar este poder presiona la tecla R.\n\
    \Hay runas dispersas cuyos poderes pueden permitirte potenciarte para romper los obstaculos en tu camino al reunirlas todas.\n\
    \\nNo dudes en pedir ayuda si lo necesitas, siempre puedes ver los controles con la tecla H \n\
    \\nAhora ve, valiente héroe, y reclama tu lugar entre las leyendas.\n\
    \\n\n Presiona Enter para continuar."

    getLine
    clearScreen

-- Pantalla de ayuda
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