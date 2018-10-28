-- | Proyecto 1 Septiembre – Diciembre 2018 El sabio del laberinto
-- | Autores:
-- | Yuni Quintero
-- | Daniel Rodriguez 14-10955

module Laberinto where
import Data.Map as M
import Data.List.Split
import Data.Maybe

-- | Aqui se define el tipo de dato Laberinto que consiste en dos constructores
-- |        - Tesoro: Que va a almacenar un String que va a describir el tesoro
-- |                  Y un Maybe Laberinto que va a almacenar el laberinto que va 
-- |                  a reemplazar al tesoro en tal caso que sea ignorado
-- |        - Trifurcacion: Este constructor va a almacenar tres laberintos que van 
-- |                        a hacer los laberintos alcanzables desde el que se esta 
-- |                        parado a traves de tres direcciones: Izquierda, Derecha
-- |                        y Recto
data Laberinto = Tesoro String (Maybe Laberinto) | Trifurcacion (Maybe Laberinto) (Maybe Laberinto) (Maybe Laberinto) deriving (Show, Read, Eq)


-- | ------------------------------------ Funciones de construccion ----------------------------

-- | sin_salida:
-- |        Entrada: Ninguna 
-- |        Salida: Laberinto 
-- |        Descripcion: Es una funcion que construye un laberinto sin salidas 
sin_salida :: Laberinto 
sin_salida = Trifurcacion Nothing Nothing Nothing

-- | cons_tesoro:
-- |        Entrada: String que es la descripcion
-- |                 Laberinto que es el laberinto con el cual esta relacionado el tesoro
-- |        Salida:  Laberinto, especificamente de tesoro pues se usa el constructor de tesoro
-- |        Descripcion: Esta funcion se encarga de construir los tesoros que puede haber en el
-- |                     laberinto.
cons_tesoro :: String -> Laberinto -> Laberinto
cons_tesoro x y = Tesoro x (Just y)

-- | ------------------------------------------------------------------------------------------------
-- | ------------------------------------- Funciones de acceso -------------------------------------
-- | acc_izq:
-- |        Entrada: Laberinto, que es el laberinto desde el cual vamos a tomar la direccion izquierda
-- |        Salida:  Maybe Laberinto, pues puede terminar en otro laberinto o en una pared 
-- |                 que es representado con Nothing 
-- |        Descripcion: Esta funcion permite acceder al laberinto que se encuentra a la izquierda 
-- |                     desde el laberinto actual
acc_izq :: Laberinto -> Maybe Laberinto
acc_izq (Trifurcacion x _ _) = x

-- | acc_der:
-- |        Entrada: Laberinto, que es el laberinto desde el cual vamos a tomar la direccion derecha
-- |        Salida:  Maybe Laberinto, pues puede terminar en otro laberinto o en una pared 
-- |                 que es representado con Nothing
-- |        Descripcion: Esta funcion permite acceder al laberinto que se encuentra a la derecha
-- |                     desde el laberinto actual
acc_der :: Laberinto -> Maybe Laberinto
acc_der (Trifurcacion _ x _) = x

-- | acc_rec:
-- |        Entrada: Laberinto, que es el laberinto desde el cual vamos a tomar la direccion recto
-- |        Salida:  Maybe Laberinto, pues puede terminar en otro laberinto o en una pared 
-- |                 que es representado con Nothing
-- |        Descripcion: Esta funcion permite acceder al laberinto que se encuentra siguiendo recto 
-- |                     desde el laberinto actual
acc_rec :: Laberinto -> Maybe Laberinto
acc_rec (Trifurcacion _ _ x) = x

-- | --------------------------------------------------------------------------------------------------
-- | conexion:
-- |        Entrada: Laberinto, que es el laberinto desde donde se va a partir
-- |                 Laberinto, que es el laberinto donde se quiere llegar 
-- |                 String, que es la direccion que se va a tomar desde el primer laberinto 
-- |                 para llegar al segundo.
-- |        Salida:  Laberinto, que es el laberinto que consigo trae la conexion con el segundo
-- |                 laberinto
-- |        Descripcion: Esta funcion se encarga de hacer la conexion entre dos laberintos
conexion :: Laberinto -> Laberinto -> String -> Laberinto
conexion x y z
    |   z=="izquierda"  =  Trifurcacion (Just y) (acc_der x) (acc_rec x)
    |   z=="derecha"    = Trifurcacion (acc_izq x) (Just y) (acc_rec x)
    |   z=="recto"      = Trifurcacion (acc_izq x) (acc_der x) (Just y)

-- | convert:
-- |        Entrada: Maybe a, que es un tipo Maybe de un cualquier tipo a 
-- |        Salida:  a, es el tipo que en realidad es 
-- |        Descripcion: Solo es una funcion auxiliar para llevar los tipos Maybe Laberinto a Laberinto
convert :: Maybe a -> a
convert x = case x of Nothing -> error "Se encontro una pared."
                      Just x -> x

-- | recorrer:
-- |        Entrada: Laberinto, que es el laberinto desde donde voy a comenzar a recorrer 
-- |                 [String], que es la lista que contiene la ruta a seguir 
-- |        Salida:  Laberinto, es el laberinto que se encuentra al final de la ruta
-- |        Descripcion: Esta funcion se encarga de recorrer la ruta en un laberinto dado
recorrer :: Laberinto -> [String] -> Laberinto
recorrer x [] = x
recorrer x (y:ys)
    |   y=="izquierda"  = recorrer (convert $ acc_izq x) ys
    |   y=="derecha"  = recorrer (convert $ acc_der x) ys
    |   y=="recto"  = recorrer (convert $ acc_rec x) ys
            