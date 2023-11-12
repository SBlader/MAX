import System.Process ( callCommand )
import System.Random ( mkStdGen, StdGen, randomRs )
import MapGen
import System.Environment ( getArgs )
import System.IO ( hSetBuffering, stdin, BufferMode(NoBuffering) ) 
import Data.List ( delete, nub )
import GHC.Float.RealFracMethods ( roundFloatInt )


{- FUNCIONAMIENTO PRINCIPAL -}
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
        if (n <= 4) then -- Si el mapa es muy chico, es mejor pedir generar de nuevo
            putStrLn "Por favor escoge un n mayor o igual a 4 para tener una buena experiencia de juego..." 
        else do 
            let (posTesoro, posRunas) = (genTesoro rand n, nub $ filter (\ x -> (/=) (0,0) x && (/=) posTesoro x) $ foldl (\ acc x -> anyPos x n : acc) [] $ take ((*2) . roundFloatInt . log . fromIntegral $ (n)) $ fmap mkStdGen (randomRs (1,1000) rand)) -- Generar las posiciones de runas y el tesoro
            let (cantidadPozos, cantidadObstaculos) = (round (fromIntegral n * fromIntegral n * 0.06) :: Int, 3 * cantidadPozos :: Int) -- Generar la cantidad de los obstaculos
            -- Generar el mapa
            let mapa = makeMapa n posRunas posTesoro cantidadPozos cantidadObstaculos rand
            tutorial
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
        if getKey action == 'r' then loop score (makeMapa n posRunas tesoro cantidadPozos cantidadLava (strongRandom 50 rand)) (x,y) posRunas tesoro n m cantidadPozos cantidadLava (strongRandom 100 rand) -- Reintentar
        else if getKey action == 'h' then do help; loop score mapa (x,y) posRunas tesoro n m cantidadPozos cantidadLava rand -- Menu de ayuda luego itera nuevamente
        else if newCelda == Runa then loop ((*2) <$> (if isNada score then Score 1 else score)) (cambiarCelda mapa (x,y) Camino) (getMov action) (delete (getMov action) posRunas) tesoro n m cantidadPozos cantidadLava rand -- Si llegas a una runa score = score*2
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

-- Limpia la pantalla
clearScreen :: IO ()
clearScreen = callCommand "clear"

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