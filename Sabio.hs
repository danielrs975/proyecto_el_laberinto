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
get_ruta x = reverse (splitOn " " x)

-- | Funcion que genera un nuevo laberinto en memoria, Opcion 1

labNuevo :: Laberinto -> Laberinto -> [String] -> Laberinto 
labNuevo _ y [] = y
labNuevo x y (z:zs) = labNuevo sin_salida (conexion x y z) zs

-- | Funcion que ejecuta la primera opcion "Comenzr a hablar de un laberinto nuevo"
execLabNuevo :: IO()
execLabNuevo = do 
    rutaUsuario <- getLine
    let listaRuta = get_ruta rutaUsuario 
    putStrLn $ show (labNuevo sin_salida sin_salida listaRuta)


-- menu :: Maybe Laberinto -> IO()
menu (Just laberintoActual) = do 
    putStrLn $ mostrarOpciones opcionesPrincipales
    opcion <- getLine
    laberintoNuevo <- case validar opcion opcionesPrincipales of
        Just 1 -> execLabNuevo        
        Just 2 -> putStrLn "pregRuta"
        Just 3 -> putStrLn "paredAbierta"
        Just 4 -> putStrLn "derrumbe"
        Just 5 -> putStrLn "tesoroTomado"
        Just 6 -> putStrLn "tesoroHallado"
        Just 7 -> putStrLn "nameLab"
        Just 8 -> putStrLn "hablarLab"
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
        Just 1 -> execLabNuevo
        Just 2 -> putStrLn "hablarLab"
        Nothing -> do
            putStrLn ""
            putStrLn "Opción incorrecta."
            putStrLn ""
            menu Nothing 

-- submenu :: Maybe Laberinto -> IO()
-- submenu (Just lab) = do
--     putStrLn $ mostrarOpciones subopciones
--     opcion <- getLine 
--     case validar opcion subopciones of
--         Just 1 -> putStrLn "Continuar ruta"
--         Just 2 -> putStrLn "Preguntar ruta nueva"
--         Nothing -> do 
--             putStrLn ""
--             putStrLn "Opción incorrecta."
--             putStrLn ""
--             submenu

main = menu Nothing