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
    rutaUsuario <- getLine
    let listaRuta = get_ruta rutaUsuario
    let laberintoAlcanzado = recorrer x listaRuta
    case checkLab laberintoAlcanzado of 
        True -> do
            putStrLn $ show laberintoAlcanzado
            return y 
        otherwise -> submenu laberintoAlcanzado y
    
-- | Funcion que obtiene la ruta hasta la primera pared que encuentra 
obtenerRutaPared :: Maybe Laberinto -> [String] -> [String]
obtenerRutaPared Nothing y = y
obtenerRutaPared x [] = []
obtenerRutaPared x (y:ys)
    |   y=="izquierda"  = obtenerRutaPared (acc_izq $ convert x) ys
    |   y=="derecha"  = obtenerRutaPared (acc_der $ convert x) ys
    |   y=="recto"  = obtenerRutaPared (acc_rec $ convert x) ys

-- | Funcion que se encarga de ejecutar la opcion reportar pared abierta 

execParedAbierta :: Laberinto -> IO Laberinto 
execParedAbierta x = do 
    caminoUsuario <- getLine
    let listaRuta = get_ruta caminoUsuario
    let rutaRestante = obtenerRutaPared (Just x) listaRuta
    let longitudRutaRecorrida = length listaRuta - length rutaRestante - 1
    let rutaRecorrida = take longitudRutaRecorrida listaRuta
    let laberintoActual = recorrer x rutaRecorrida
    let subLaberinto = labNuevo sin_salida sin_salida (reverse rutaRestante)
    let nuevoLaberinto = labNuevo laberintoActual subLaberinto (reverse rutaRecorrida)
    return nuevoLaberinto

-- | Funcion que se encarga de ejecutar la opcion de reportar un derrumbe 
-- | en el laberinto 

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