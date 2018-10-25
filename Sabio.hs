-- | Este es el main de nuestro programa que representa un laberinto 
-- | aqui se presenta un menu que permite al usuario ejecutar las distintas 
-- | opciones que se ofrecen 

module Sabio(main) where
import System.IO 

-- | Es una lista que mantiene todas las opciones disponibles 
-- | para el usuario
opciones :: [(Int, String)]
opciones = zip [1..] ["Comenzar a hablar de un laberinto nuevo",
                      "Preguntar ruta", 
                      "Reportar pared abierta", 
                      "Reportar derrumbe",
                      "Reportar tesoro tomado",
                      "Reportar tesoro hallado",
                      "Dar nombre al laberinto",
                      "Hablar de un laberinto de nombre conocido"]

-- | Esta funcion se encarga de mostrar las opciones disponibles por pantalla 
-- | La funcion recibe una lista de pares ordenados y regresa un string con las opciones
mostrarOpciones :: [(Int, String)] -> String
mostrarOpciones xs = foldl (\acc x -> acc ++ show (fst x) ++ ". " ++ snd x ++ "\n") "" xs

-- | Funcion validar las opciones 
validar :: String -> Maybe Int
validar s = esValido (reads s)
    where esValido [] = Nothing
          esValido ((n, _):_) 
                | (n < 1) || (n > length opciones) = Nothing
                | otherwise = Just n

main :: IO()
main = do 
    putStrLn $ mostrarOpciones opciones
    opcion <- getLine
    putStrLn opcion
    case validar opcion of
        Just 1 -> labNuevo
        Just 2 -> askRuta
        Just 3 -> paredAbierta
        Just 4 -> derrumbe
        Just 5 -> tesoroTomado
        Just 6 -> tesoroHallado
        Just 7 -> nameLab
        Just 8 -> hablarLab
        Nothing -> do
            putStrLn "Opción incorrecta."
        
