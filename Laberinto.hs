module Laberinto where
import Data.Map as M

data Laberinto = Laberinto Trifurcacion Tesoro deriving (Show)

data Trifurcacion = Trifurcacion {  derecha :: Maybe Laberinto,
                                    izquierda :: Maybe Laberinto,
                                    recto :: Maybe Laberinto} deriving (Show)

data Tesoro = Tesoro {  descripcion :: String,
                        ignorar :: Maybe Laberinto} deriving (Show)

