-- | Este es el main de nuestro programa que representa un laberinto 
-- | aqui se presenta un menu que permite al usuario ejecutar las distintas 
-- | opciones que se ofrecen 

module Sabio(main) where
import System.IO 
import Laberinto
import Data.List.Split


-- | Es una lista que mantiene todas las opciones disponibles 
-- | para el usuario
opcionesPrincipales :: [(Int, String)]
opcionesPrincipales = zip [1..] ["Comenzar a hablar de un laberinto nuevo",
                      "Preguntar ruta", 
                      "Reportar pared abierta", 
                      "Reportar derrumbe",
                      "Reportar tesoro tomado",
                      "Reportar tesoro hallado",
                      "Dar nombre al laberinto",
                      "Hablar de un laberinto de nombre conocido"]
-- | Lista de opciones disponibles cuando se inicia el programa sin 
-- | ningun laberinto en memoria
opcionesIniciales = zip [1..] ["Comenzar a hablar de un laberinto nuevo", "Hablar de un laberinto nuevo"]

-- | Es una lista con las opciones cuando se selecciona Preguntar ruta 
subopciones :: [(Int, String)]
subopciones = zip [1..] ["Continuar ruta", "Preguntar ruta nueva"]

-- | Esta funcion se encarga de mostrar las opciones disponibles por pantalla 
-- | La funcion recibe una lista de pares ordenados y regresa un string con las opciones
mostrarOpciones :: [(Int, String)] -> String
mostrarOpciones xs = foldl (\acc x -> acc ++ show (fst x) ++ ". " ++ snd x ++ "\n") "" xs

-- | Funcion validar las opciones 
validar :: String -> [(Int, String)] -> Maybe Int
validar s listaOpciones = esValido (reads s)
    where esValido [] = Nothing
          esValido ((n, _):_) 
                | (n < 1) || (n > length listaOpciones) = Nothing
                | otherwise = Just n

-- | Funcion que separa un string por sus espacios y devuelve una lista con cada palabra

get_ruta :: String -> [String]
get_ruta x = splitOn " " x

-- | Funcion que genera un nuevo laberinto en memoria, Opcion 1

labNuevo :: Laberinto -> Laberinto -> [String] -> Laberinto 
labNuevo _ y [] = y
labNuevo x y (z:zs) = labNuevo sin_salida (conexion x y z) zs

-- | Funcion que ejecuta la primera opcion "Comenzar a hablar de un laberinto nuevo"
execLabNuevo :: IO Laberinto
execLabNuevo = do 
    putStrLn "Introduza la ruta: "
    rutaUsuario <- getLine
    let listaRuta = reverse $ get_ruta rutaUsuario 
    return (labNuevo sin_salida sin_salida listaRuta)

-- | Funcion que recibe un laberinto y verifica si es un tesoro o una pared
checkLab :: Laberinto -> Bool
checkLab (Tesoro _ _) = True
checkLab x
    |   x==sin_salida   =   True
    |   otherwise   =   False

-- | Funcion que le pide al usuario una ruta y empieza recorrer hasta terminar con un camino sin salida 
-- | o un tesoro 
execPregRuta :: Laberinto -> IO Laberinto
execPregRuta x = do 
    putStrLn "Introduza la ruta: "
    rutaUsuario <- getLine
    let listaRuta = get_ruta rutaUsuario
    let laberintoAlcanzado = recorrer x listaRuta
    case checkLab laberintoAlcanzado of 
        True -> do 
            putStrLn $ show laberintoAlcanzado
            return x 
        otherwise -> submenu laberintoAlcanzado x

-- | Funcion continuar ruta.
execContRuta :: Laberinto -> Laberinto -> IO Laberinto 
execContRuta x y = do 
    putStrLn "Introduza la ruta: "
    rutaUsuario <- getLine
    let listaRuta = get_ruta rutaUsuario
    let laberintoAlcanzado = recorrer x listaRuta
    case checkLab laberintoAlcanzado of 
        True -> do
            putStrLn $ show laberintoAlcanzado
            return y 
        otherwise -> submenu laberintoAlcanzado y
    
-- | Funcion que obtiene la ruta hasta la primera pared que encuentra 
obtenerRutaPared :: Maybe Laberinto -> [String] -> String -> [String]
obtenerRutaPared Nothing y z = [z] ++ y 
obtenerRutaPared x (y:ys) z
    |   y=="izquierda"  = obtenerRutaPared (acc_izq $ convert x) ys y
    |   y=="derecha"  = obtenerRutaPared (acc_der $ convert x) ys y
    |   y=="recto"  = obtenerRutaPared (acc_rec $ convert x) ys y

-- | Funcion que obtiene la lista de los laberintos que estan antes de encontrar una pared
laberintosRuta :: Laberinto ->  [String] -> [Laberinto]
laberintosRuta x [] = []
laberintosRuta x (y:ys)
    |   y=="izquierda"  = [convert $ acc_izq x] ++ laberintosRuta (convert $ acc_izq x) ys 
    |   y=="derecha"  = [convert $ acc_der x] ++ laberintosRuta (convert $ acc_der x) ys 
    |   y=="recto"  = [convert $ acc_rec x] ++ laberintosRuta (convert $ acc_rec x) ys

-- | Funcion que reconstruye el laberinto a partir de una lista de los laberintos recorridos
reconsLab :: [Laberinto] -> Laberinto -> [String] -> Laberinto
reconsLab [] z [] = z
reconsLab (x:xs) z (w:ws) = reconsLab xs (conexion x z w) ws

-- | Funcion que se encarga de ejecutar la opcion reportar pared abierta 

execParedAbierta :: Laberinto -> IO Laberinto
execParedAbierta x = do 
    putStrLn "Introduzca la ruta: "
    caminoUsuario <- getLine
    -- Estos dos comandos obtienen la ruta que falta para completar el camino 
    let listaRuta = get_ruta caminoUsuario
    let rutaRestante = obtenerRutaPared (Just x) listaRuta (head listaRuta)
    -- Aqui se obtiene el conector entre el sublaberinto y el laberinto principal
    let conector = head rutaRestante
    let rutaRestante2 = drop 1 rutaRestante
    -- Aqui se obtiene la ruta recorrida hasta que se encuentra la pared
    let rutaRecorrida = take (length listaRuta - length rutaRestante) listaRuta
    -- Estos son los laberintos que recorri hasta llegar a una pared 
    let laberintosRecorridos = [x]++(laberintosRuta x rutaRecorrida)
    putStrLn $ show laberintosRecorridos
    -- Obtenemos el laberinto en el cual vamos a empezar a recorrer la nueva ruta 
    let laberintoActual = recorrer x rutaRecorrida
    -- Obtenemos el sublaberinto para tal ruta 
    let sublaberinto = if length rutaRestante2 > 0 then labNuevo sin_salida sin_salida (reverse rutaRestante2) else sin_salida
    let sublaberinto2 = conexion (last laberintosRecorridos) sublaberinto conector
    -- Aqui conectamos el sublaberinto con el laberinto principal 
    let laberintoNuevo = reconsLab (drop 1 $ reverse laberintosRecorridos) sublaberinto2 (reverse (rutaRecorrida))
    putStrLn $ show laberintoNuevo
    return laberintoNuevo

-- | Esta funcion se encarga de eliminar el sublaberinto que existe 
-- | en la direccion indicada 
eliminarSubLaberinto :: Laberinto -> String -> Laberinto
eliminarSubLaberinto x y 
    |   y=="izquierda"  = Trifurcacion Nothing (acc_der x) (acc_rec x) 
    |   y=="derecha"  = Trifurcacion (acc_izq x) Nothing (acc_rec x)
    |   y=="recto"  = Trifurcacion (acc_izq x) (acc_der x) Nothing

-- | Funcion que se encarga de ejecutar la opcion de reportar un derrumbe 
-- | en el laberinto 

execDerrumbe :: Laberinto -> IO Laberinto
execDerrumbe x = do 
    -- Obtenemos las entradas dadas por el usuario 
    putStrLn "Introduza la ruta: "
    rutaUsuario <- getLine
    let listaRuta = get_ruta rutaUsuario
    putStrLn "Introduzca una direccion: "
    direccion <- getLine 
    -- Obtenemos el laberinto al cual se puede llegar por el camino provisto por el usuario
    let laberintoActual = recorrer x listaRuta
    let sublaberinto = eliminarSubLaberinto laberintoActual direccion
    return sublaberinto 





-- menu :: Maybe Laberinto -> IO()
menu (Just laberintoActual) = do
    putStrLn $ mostrarOpciones opcionesPrincipales
    opcion <- getLine
    laberintoNuevo <- case validar opcion opcionesPrincipales of
        Just 1 -> execLabNuevo        
        Just 2 -> execPregRuta laberintoActual
        Just 3 -> execParedAbierta laberintoActual
        Just 4 -> execDerrumbe laberintoActual
        -- Just 5 -> putStrLn "tesoroTomado"
        -- Just 6 -> putStrLn "tesoroHallado"
        -- Just 7 -> putStrLn "nameLab"
        -- Just 8 -> putStrLn "hablarLab"
        Nothing -> do
            putStrLn ""
            putStrLn "Opción incorrecta."
            putStrLn ""
            return laberintoActual
    menu (Just laberintoNuevo)

menu Nothing = do 
    putStrLn $ mostrarOpciones opcionesIniciales
    opcion <- getLine 
    case validar opcion opcionesIniciales of 
        Just 1 -> do
            temp <- execLabNuevo
            menu (Just temp)
        -- Just 2 -> putStrLn "hablarLab"
        Nothing -> do
            putStrLn ""
            putStrLn "Opción incorrecta."
            putStrLn ""
            menu Nothing 


-- submenu :: Laberinto -> Laberinto -> IO Laberinto
submenu laberintoActual laberintoInicial = do
    putStrLn $ mostrarOpciones subopciones
    opcion <- getLine 
    case validar opcion subopciones of
            Just 1 -> execContRuta laberintoActual laberintoInicial
            Just 2 -> execPregRuta laberintoInicial
            Nothing -> do 
                putStrLn ""
                putStrLn "Opción incorrecta."
                putStrLn ""
                return laberintoActual

main = menu Nothing