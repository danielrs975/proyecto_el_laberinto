module Laberinto where
import Data.Map as M

data Laberinto = Laberinto Trifurcacion Tesoro deriving (Show)

data Trifurcacion = Trifurcacion {  derecha :: Maybe Laberinto,
                                    izquierda :: Maybe Laberinto,
                                    recto :: Maybe Laberinto} deriving (Show)

data Tesoro = Tesoro {  descripcion :: String,
                        ignorar :: Maybe Laberinto} deriving (Show)

sin_salida :: Trifurcacion
sin_salida = Trifurcacion{derecha=Nothing, izquierda=Nothing, recto=Nothing}

desc_tesoro :: String -> Maybe Laberinto -> Tesoro
desc_tesoro x y = Tesoro{descripcion=x, ignorar=y}

cons_trif :: Trifurcacion -> Maybe Laberinto -> String -> Trifurcacion
cons_trif x y z
    | z=="derecha" = Trifurcacion{derecha=y, izquierda=izquierda x, recto=recto x}
    | z=="izquierda" = Trifurcacion{derecha=derecha x, izquierda=y, recto=recto x}
    | z=="recto" = Trifurcacion{derecha=derecha x, izquierda=izquierda x, recto=y}

acc_izq :: Maybe Laberinto -> Maybe Laberinto
acc_izq x = (Trifurcacion x) izquierda

