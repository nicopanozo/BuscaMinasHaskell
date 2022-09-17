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
Un pequeño manual de usuario será entregado por correo, gracias por las enseñanzas en la materia inge :)
-}

module BuscaMinasV2 where

import Data.List ( (\\), zipWith4, nub )
import ModuloRandom ( cuadriculaR )

-- (\\) :: Eq a => [a] -> [a] -> [a]
-- zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
-- nub :: Eq a => [a] -> [a]

{-Primero se creó un tipo de datos "Point para usarlo en las funciones -}

type Point = (Int,Int)

{-Esta funcion es una variante de zip que combina 3 listas por elementos usando una funcion f-}

zipConjunto3 :: (a -> a -> a -> a) -> a -> [a] -> [a]

zipConjunto3 f y xs = zipWith3 f (y:xs) xs (tail xs ++ [y])

{-Se crea una clase Agregar 3 con funciones como add3, cero que son bastante simples y la funcion compensacion que usa la funcion zipConjunto3 -}
class Agregar3 a where
  add3 :: a -> a -> a -> a
  cero :: a
  agregarConjunto :: [a] -> [a]
  agregarConjunto = zipConjunto3 add3 cero
  
{-Se tienen 2 instancias para la clase Agregar 3, una con la suma de 3 Ints para add3 donde cero = 0 y otra donde se usa la funcion zipWith3 que devuelve una lista de listas aplicandoles una funcion-}
{-Tambien se usa la funcion repeat para cero, que crea una lista infinita donde todos los elementos son el primer argumento-}
instance Agregar3 Int where
  add3 n m p = n+m+p
  cero       = 0

instance Agregar3 a => Agregar3 [a] where
  add3 = zipWith3 add3
  cero = repeat cero

-- Boolean matrix to numeric matrix; True to 1, 

{-
Matriz de Bools a matriz numérica
True a 1
False a 0
-}

matrizNumerica :: [[Bool]] -> [[Int]]
matrizNumerica = map (map (\a -> if a then 1 else 0))


{-Esta funcion calcula el numero de casillas adyacentes ocupadas a partir de la cuadrícula de ocupación lista de listas de bools-}
{-Aqui usamos la funcion agregarConjunto que fue definida en la clase Agregar 3, la cual recibe add3 y cero usamos composicion de funciones y la funcion matrizNumerica-}

adyacentes :: [[Bool]] -> [[Int]]

adyacentes = agregarConjunto . map agregarConjunto . matrizNumerica


pad :: Int -> String -> String

pad n st
  | len <= n = st ++ replicate (n - len) ' ' 
  | otherwise = take n st
    where
    len = length st

-- actualizar list xs at index n to have value f (xs!!n)
-- Handles out of range indices

actualizar :: Int -> (a -> a) -> [a] -> [a]

actualizar n f xs = front ++ rear
  where
		  (front,rest) = splitAt n xs
		  rear = case rest of
			            []	-> []
			            (h:t)	-> f h:t
			
-- actualizar an array to have value x at position (n,m)			
 
actualizarArreglo :: Point -> a -> [[a]] -> [[a]]

actualizarArreglo (n,m) x xss = actualizar n (actualizar m (const x)) xss


mostrarJuego :: [[Bool]] -> [[Bool]] -> [[Int]] -> String

mostrarJuego zss xss yss 
           = "\n   " ++ take (length (head yss)) ['a' .. 'z'] ++ "\n" ++
             concat (zipWith4 f [0 .. length yss - 1] zss xss yss) ++"\n"
	     where
	     f n es ms ns 
	       = pad 3 (show n) ++ concat (zipWith3 mostrarValor es ms ns) ++ "\n" 

-- Esta funcion muestra el valor en cada cuadr

mostrarValor :: Bool -> Bool -> Int -> String

mostrarValor mostrar marcado n 
	= if marcado then "X"
	     else if not mostrar then "#"
                 else if n==0 then " "
		     else show n

{-Funcion para la búsqueda de arrays -}

(%%) :: [[a]] -> Point -> a

xss%%(p,q) = xss!!p!!q


jugar :: Int -> Int -> IO ()

jugar minas dimension = 
   jugarCuadricula cuadricula count mostrar marcado

   where

   cuadricula = cuadriculaR minas dimension dimension
   count      = adyacentes cuadricula			
   mostrar    = map (map (const False)) cuadricula
   marcado    = map (map (const False)) cuadricula
   
jugarCuadricula :: [[Bool]] -> [[Int]] -> [[Bool]] -> [[Bool]] -> IO ()

jugarCuadricula cuadricula count mostrar marcado =
     do { putStr (mostrarJuego mostrar marcado count) ;
          (opcion,point) <- getInput dimension ;
	  case opcion of
	    'q' -> return ()
	    'h' -> do { putStr ayuda ; jugarCuadricula cuadricula count mostrar marcado }
	    'm' -> jugarCuadricula cuadricula count mostrar (actualizarArreglo point True marcado)
	    'u' -> jugarCuadricula cuadricula count mostrar (actualizarArreglo point False marcado)
	    'r' -> if cuadricula%%point 
	             then (do { putStr "\n\n¡¡¡¡BOOOOOOOOOM!!! (T.N.T()--✹\n\nPerdiste vuelve a intentarlo (⌣̩̩́_⌣̩̩̀) ☟\n\n"; return () })
	             else
	                (jugarCuadricula cuadricula count 
			              (descubrirCierre count point mostrar)
	                              marcado)
	}
  
	where dimension = length cuadricula

ayuda :: String

ayuda
  = "\n\n Informacion Util\n\n\
    \ q\tSalir\n\
    \ h\tAyuda\n\
    \ m0a\tMarcar Punto Seleccionado\n\
    \ u0a\tQuitar Marca \n\
    \ r0a\tRevelar\n\\n"

{-
Pone a un valor entero el el rango 0 .. r-1
-}

encajarRango :: Int -> Int -> Int

encajarRango r val
  | 0<=val && val<r	= val
  | val<0		= 0
  | val>=r 		= r-1


{-
Obtenemos los inputs que da a escoger entre una opcion y una celda, en caso de ayuda o salir no se requiere informacion de celdas.
-}

getInput :: Int -> IO (Char,Point)

getInput dimension =
  do {    opcion <- getChar ;
	  if elem opcion "smurat"
	  then 
	   do {
           filaCh <- getChar ;				-- obtenemos fila
	   colCh <- getChar ;				        -- obtenemos columna
	   let { fila = encajarRango dimension (fromEnum filaCh - fromEnum '0') } ; 
	   let { col = encajarRango dimension (fromEnum colCh - fromEnum 'a') } ;
	   let { point = (fila,col) } ;
	   return (opcion,point)
	      }
	  else 			
	  do {
	   let { dummy = (0,0) } ;
	   return (opcion,dummy)
	      }
     }

{--}

cierre :: [[Int]] -> Point -> [Point]

cierre count point = hacerCierre count point []

{-
Los vecinos de un punto en buscaminas
-}

vecinos :: [[Int]] -> Point -> [Point]

vecinos count (p,q)
  = filter enCuadricula [ (p-1,q-1), (p-1,q), (p-1,q+1),
                        (p,q-1),   (p,q),   (p,q+1),
		                    (p+1,q-1), (p+1,q), (p+1,q+1) ]
    where
    enCuadricula (s,t) = 0<=s && s <= filas &&
                   0<=t && t <= columnas
    filas = length count - 1
    columnas = length (head count) -1


hacerCierre :: [[Int]] -> Point -> [Point] -> [Point]

hacerCierre count point evadir
  | count%%point /= 0	= [point]
  | otherwise	
    = point : hacerCierreLista count veci (point:evadir)
      where
      veci = vecinos count point

hacerCierreLista :: [[Int]] -> [Point] -> [Point] -> [Point]

hacerCierreLista count [] evadir = []

hacerCierreLista count (point: points) evadir
  = next ++ hacerCierreLista count points (evadir ++ next)
    where
    next = if elem point evadir
           then [point]
	   else hacerCierre count point evadir

-- Descubrir todos los puntos en el cierre

descubrirCierre :: [[Int]] -> Point -> [[Bool]] -> [[Bool]]

descubrirCierre count point 
  = foldr (.) id $ 
    map (flip actualizarArreglo True) (cierre count point)



{-
Base para el desarrollo del juego
https://es.wikipedia.org/wiki/Buscaminas

Muestra de importacion y uso de algunas funciones de Data.List
http://learnyouahaskell.com/modules

Mas informacion sobre Data.List y la funcion (\\)
https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html

El juego en c

https://parzibyte.me/blog/2020/06/08/buscaminas-c-juego/

Algunos ejemplos de buscaminas en haskell que sirvieron de base

https://github.com/ilyazuev/haskell_minesweeper/blob/master/src/Main.hs
https://github.com/gdlow/haskell-minesweeper/blob/master/haskell-minesweeper.hs
https://github.com/endymion64/HaskellMines/blob/master/MyBoard.hs


Ayuda para generar un numero random en haskell en el archivo MinasRandom.hs (Tambien me sirvieron los archivos del grupo de WPP)
https://stackoverflow.com/questions/8416365/generate-a-random-integer-in-a-range-in-haskell-without-a-seed

-}