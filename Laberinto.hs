-- Proyecto 1 Septiembre – Diciembre 2018 El sabio del laberinto
-- Autores:
-- Yuni Quintero
-- Daniel Rodriguez

module Laberinto where
import Data.Map as M

-- Tipo de datos que almacena conocimientos del sabio sobre el laberinto
-- Atributos: Trifurcacion y Tesoro
data Laberinto = Laberinto  {trif :: Trifurcacion,
                            tesoro :: Tesoro} deriving (Show)

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
                        ignorar :: Maybe Laberinto} deriving (Show)

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
desc_tesoro :: String -> Maybe Laberinto -> Tesoro
desc_tesoro x y = Tesoro{descripcion=x, ignorar=y}

-- Funcion que recibe una Trifurcacion, un laberitno y un indicador de cual camino
-- los relacion y retorna un Trifurcacion que indica que dicho camino conduce a 
-- dicho laberitno
-- Parametros: x una Trifurcacion, y un Maybe Laberinto, z un String que es nuestro
-- indicador
cons_trif :: Trifurcacion -> Maybe Laberinto -> String -> Trifurcacion
cons_trif x y z
    | z=="derecha" = Trifurcacion{derecha=y, izquierda=izquierda x, recto=recto x}
    | z=="izquierda" = Trifurcacion{derecha=derecha x, izquierda=y, recto=recto x}
    | z=="recto" = Trifurcacion{derecha=derecha x, izquierda=izquierda x, recto=y}

-- Funciones de Acceso

-- Funcion que recibe un laberinto y retorna el laberinto que comienza al voltear
-- a la izquierda
acc_izq :: Maybe Laberinto -> Maybe Laberinto
acc_izq x = (trif x) izquierda

