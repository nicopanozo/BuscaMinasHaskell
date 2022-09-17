{-
-------------------------------------------------------------------
______ _   _ _____ _____   ___  ___  ________ _   _   ___   _____ 
| ___ \ | | /  ___/  __ \ / _ \ |  \/  |_   _| \ | | / _ \ /  ___|
| |_/ / | | \ `--.| /  \// /_\ \| .  . | | | |  \| |/ /_\ \\ `--. 
| ___ \ | | |`--. \ |    |  _  || |\/| | | | | . ` ||  _  | `--. \
| |_/ / |_| /\__/ / \__/\| | | || |  | |_| |_| |\  || | | |/\__/ /
\____/ \___/\____/ \____/\_| |_/\_|  |_/\___/\_| \_/\_| |_/\____/ 
                                                                  
-------------------------------------------------------------------
 _   _ _           _             ____                           
| \ | (_) ___ ___ | | __ _ ___  |  _ \ __ _ _ __   ___ _______  
|  \| | |/ __/ _ \| |/ _` / __| | |_) / _` | '_ \ / _ \_  / _ \ 
| |\  | | (_| (_) | | (_| \__ \ |  __/ (_| | | | | (_) / / (_) |
|_| \_|_|\___\___/|_|\__,_|___/ |_|   \__,_|_| |_|\___/___\___/ 
                                                                
-------------------------------------------------------------------
-}

module ModuloRandom ( cuadriculaR ) where

import System.IO.Unsafe ( unsafePerformIO )
import Data.List ( insert , nub )
import System.Random

{-
Generamos una combinacion random de y elementos desde x
suponemos que y <= x
el resultado no debe tener duplicados y esta en orden ascendente
-}

opciones :: Int -> Int -> Int -> [Int]

opciones k x y
  = fst (opcionesAux ([],rnds))
    where
    
    opcionesAux :: ([Int],[Int]) -> ([Int],[Int])
    opcionesAux (cs,(r:rs))
      | length cs >= y 	= (cs,[])
      | otherwise	= opcionesAux (nub (insert r cs) , rs)
    -- randomRs :: RandomGen g => (a, a) -> g -> [a] esta funcion es de System.Random, igual mkStdGen
    rnds :: [Int]
    rnds = randomRs (0::Int,x-1) (mkStdGen k)
    
{-
Se elige un valor una vez por sesión, el valor se mantiene si se sigue en la misma sesión.
-}

valorSesion :: Int
valorSesion = unsafePerformIO (getStdRandom (randomR (0, 9)))

{-
Primero creamos esta cuadricula para el juego y despues le añadiremos el valor por sesion para que cada sesion sea unica

- Se crea una matriz con opciones de l * h
- l = length de la fila
- h = height de la columna
-}

crearCuadriculaR :: Int -> Int -> Int -> Int -> [[Bool]]

crearCuadriculaR valor n l h = pad
    where
    
    crearMatriz :: Int -> [Int] -> [[Bool]]

    crearMatriz i cs
      | cs==[]		= []
      | otherwise
        = convert first : crearMatriz (i+1) rest
	  where
	  (first,rest) = span ((==i).(flip div l)) cs
	  convert ns 
	     = map check [0 .. l-1]
	       where
	       check n = elem n [ x `mod` l | x<-ns ]

    filas = crearMatriz 0 (opciones valor (l*h) n)
     
    pad = filas ++ replicate (h - length filas) (replicate l False)

-- Cuadricula random con un valor generado por sesion

cuadriculaR :: Int -> Int -> Int -> [[Bool]]

cuadriculaR = crearCuadriculaR valorSesion 


