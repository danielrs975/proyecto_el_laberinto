-- Proyecto 1 Septiembre – Diciembre 2018 El sabio del laberinto
-- Autores:
-- Yuni Quintero
-- Daniel Rodriguez 14-10955

module Laberinto where
import Data.Map as M
import Data.List.Split
import Data.Maybe

-- Tipo de datos que almacena conocimientos del sabio sobre el laberinto
-- Atributos: Trifurcacion y Tesoro
data Laberinto = Laberinto  {trif :: Trifurcacion,
                             tesoro :: Maybe Tesoro}

-- Instancia de show para el tipo de dato Laberinto 
instance Show Laberinto where
    show Laberinto {trif=x, tesoro=y} = "El laberinto es el siguiente: " ++ show x

-- Tipo de datos que almacena conocimientos del sabio sobre una trifurcacion
-- del laberinto
-- Atributos:
-- derecha: un Maybe Laberinto que indica lo alcanzable al voltear a la derecha
-- izquierda: un Maybe Laberinto que indica lo alcanzable al voltear a la izquierda
-- recto: un Maybe Laberinto que indica lo alcanzable al seguir recto
data Trifurcacion = Trifurcacion {  derecha :: Maybe Laberinto,
                                    izquierda :: Maybe Laberinto,
                                    recto :: Maybe Laberinto} deriving (Show)

-- Tipo de datos que almacena conocimientos del sabio sobre un tesoro
-- Atributos:
-- descripcion: un String con la descripcion del tesoro
-- ignorar: un Maybe Laberinto que indica que encontraran si se pasa por alto
-- al tesoro                                    
data Tesoro = Tesoro {  descripcion :: String,
                        ignorar :: Maybe Laberinto}

-- Funciones de Construccion

-- Funcion que retorna un camino sin salida
-- Parametros: Void
-- Salida: Trifurcacion donde todos los caminos conducen a Nothing
sin_salida :: Trifurcacion
sin_salida = Trifurcacion{derecha=Nothing, izquierda=Nothing, recto=Nothing}

-- Funcion que recibe una descripcion de un tesoro y un laberinto indicando
-- que encontrarn si este se pasa por alto
-- Parametros: x un String con la descripcion del tesoro, y un Maybe Laberinto
-- que indica el laberinto a encontrar si se ignora el tesoro
-- Salida: objeto Tesoro
desc_tesoro :: String -> Laberinto -> Tesoro
desc_tesoro x y = Tesoro{descripcion=x, ignorar=Just y}


-- Funcion que recibe una Trifurcacion, un laberitno y un indicador de cual camino
-- los relacion y retorna un Trifurcacion que indica que dicho camino conduce a 
-- dicho laberitno
-- Parametros: x una Trifurcacion, y un Maybe Laberinto, z un String que es nuestro
-- indicador
cons_trif :: Trifurcacion -> Laberinto -> String -> Trifurcacion
cons_trif x y z
    | z=="derecha" = Trifurcacion{derecha=Just y, izquierda=izquierda x, recto=recto x}
    | z=="izquierda" = Trifurcacion{derecha=derecha x, izquierda= Just y, recto=recto x}
    | z=="recto" = Trifurcacion{derecha=derecha x, izquierda=izquierda x, recto=Just y}

-- Funciones de Acceso

-- Funcion que recibe un laberinto y retorna el laberinto que comienza al voltear
-- a la izquierda
-- Parametros: x un Laberinto
-- Salida: El laberinto al voltear a la izquierda

-- Idea transformar lo que devuelve acceder en las direcciones a solo laberinto 
-- si encuentra pared devolver mensaje que diga encontro una pared
acc_izq :: Laberinto -> Laberinto
acc_izq x = case y of Nothing -> error "Se encontro una pared"
                      Just y -> y 
            where y = izquierda $ trif x

-- Funcion que recibe un laberinto y retorna el laberinto que comienza al voltear
-- a la derecha
-- Parametros: x un Laberinto
-- Salida: El laberinto al voltear a la derecha
acc_der :: Laberinto -> Laberinto
acc_der x =  case y of Nothing -> error "Se encontro una pared"
                       Just y -> y 
             where y = derecha $ trif x


-- Funcion que recibe un laberinto y retorna el laberinto que comienza al seguir recto
-- Parametros: x un Laberinto
-- Salida: El laberinto al seguir recto
acc_rec :: Laberinto -> Laberinto
acc_rec x =  case y of Nothing -> error "Se encontro una pared"
                       Just y -> y
             where y = recto $ trif x

get_ruta :: String -> [String]
get_ruta x = splitOn " " x

-- get_lab_head :: Laberinto -> [String] -> Laberinto
-- get_lab_head x [] = x
-- get_lab_head x (y:ys)
    -- | y == "derecha" = get_lab_head (acc_der x) ys
    -- | y == "izquierda" = get_lab_head (acc_izq x) ys
    -- | y == "recto" = get_lab_head (acc_rec x) ys
-- 
-- acc_ruta :: Laberinto -> String -> Laberinto
-- acc_ruta x y = get_lab_head x $ get_ruta y