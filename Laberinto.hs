-- Proyecto 1 Septiembre – Diciembre 2018 El sabio del laberinto
-- Autores:
-- Yuni Quintero
-- Daniel Rodriguez 14-10955

module Laberinto where
import Data.Map as M
import Data.List.Split
import Data.Maybe

data Laberinto = Tesoro String (Maybe Laberinto) | Trifurcacion (Maybe Laberinto) (Maybe Laberinto) (Maybe Laberinto) deriving (Show)

instance Show Laberinto where
    Show (Trifurcacion x y z) = show $ convert x 

sin_salida :: Laberinto 
sin_salida = Trifurcacion Nothing Nothing Nothing

cons_tesoro :: String -> Laberinto -> Laberinto
cons_tesoro x y = Tesoro x (Just y)

acc_izq :: Laberinto -> Maybe Laberinto
acc_izq (Trifurcacion x _ _) = x

acc_der :: Laberinto -> Maybe Laberinto
acc_der (Trifurcacion _ x _) = x

acc_rec :: Laberinto -> Maybe Laberinto
acc_rec (Trifurcacion _ _ x) = x

conexion :: Laberinto -> Laberinto -> String -> Laberinto
conexion x y z
    |   z=="izquierda"  =  Trifurcacion (Just y) (acc_der x) (acc_rec x)
    |   z=="derecha"    = Trifurcacion (acc_izq x) (Just y) (acc_rec x)
    |   z=="recto"      = Trifurcacion (acc_izq x) (acc_der x) (Just y)

convert :: Maybe a -> a
convert x = case x of Nothing -> error "Se encontro una pared"
                      Just x -> x

recorrer :: Laberinto -> [String] -> Laberinto
recorrer x [] = x
recorrer x (y:ys)
    |   y=="izquierda"  = recorrer (convert $ acc_izq x) ys
    |   y=="derecha"  = recorrer (convert $ acc_der x) ys
    |   y=="recto"  = recorrer (convert $ acc_rec x) ys
            