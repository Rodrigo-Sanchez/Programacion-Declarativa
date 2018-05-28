{-Programación Declarativa 2018-2-}

import Arreglos
import Control.Monad(forM, guard, when)
import Control.Parallel.Strategies
import System.Environment(getArgs)   
import System.Random
import System.Exit

--Dado un entero n mayor a 5 devuelve la terna [(q1,q2,q3)]
--tales que q1 + q2 + q3 = n y cada qj es un número primo.
goldbach :: Int -> [(Int, Int, Int)]
goldbach n = do 
                guard (n > 5) >> return (head [(q1,q2,q3) | q1 <- primos, q2 <- primos, q3 <- primos, q1+q2+q3==n])
                    where primos = listaPrimos 2 (n-2)

--Función que regresa una lista de números primos contenido en el intervalo de [a,b]
listaPrimos :: Int -> Int -> [Int]
listaPrimos a b = takeWhile (<= b) $ dropWhile (< a) $ impar [2..]
    where impar (n:ns) = n:impar [m | m <- ns, mod m n /= 0]

--El tipo Arreglo forma parte de la clase Functor.
instance Functor Arreglo where
    fmap g (Arr f n) = Arr (\x->g (f x)) n

newtype State s a = ST (s->(a,s))
type Stack a = [a]

--Con la mónada de estados, se implementan la función minArrState
--minArrState :: Arreglo Int -> State Int (Int, Int) --LA DE MOI
--minArrState :: Ord a => Arreglo a -> Int->Int --LA DEL PDF
minArrState arr = error "minArrState"
                {-do (m,i) <- get;
                    if (i == size arr)
                        then return m;
                        else let r = arr[i];
                        if (r < m)
                            then put (r,i+1);
                                 minArrState arr;
                            else put (m, i+1);
                                 minArrState arr;-}

--Con la mónada de estados, se implementan la función selectionSortState
selectionSortState :: Ord a => Arreglo a -> Arreglo a
selectionSortState arr = error "selectionSortState"

--Devuelve el estado actual.
getState = ST $ \s -> (s,s)

--Actualiza el estado.
updState s = ST $ \_ -> ((),s)

runState::State s a->s->(a,s)
runState (ST st) s = st s

instance Functor (State s) where
   fmap = error "fmap"

instance Applicative (State s) where
   pure = error "pure"
   (<*>) = error "<*>"

instance Monad (State s) where
   return x = ST (\s -> (x,s))
   st >>= f = ST (\s -> let (a,s1) = runState st s in runState (f a) s1)

--Dado un xs, devuelve (la longitud de xs, el número de veces que aparece 1 en xs)
length1 :: [Int] -> State Int Int
length1 l = length' l (length l)
            where length' l m = case l of
                                    []     -> ST (\n -> (m, n))
                                    (x:xs) -> do
                                                if x==1
                                                  then do
                                                         ST (\n -> ((), n+1));
                                                         length' xs m
                                                  else do
                                                         length' xs m

newtype Continuation r a = Cont ((a -> r) -> r)

runCont :: Continuation r a -> (a -> r) -> r
runCont (Cont c) k = c k 

instance Functor (Continuation r) where
   fmap = error "fmap"

instance Applicative (Continuation r) where
   pure = error "pure"
   (<*>) = error "(<*>)"

--Los cómputos CPS son una mónada.
instance Monad (Continuation r) where
   return x = Cont $ \k -> k x
   ka >>= f = Cont $ \k -> runCont ka $ \a -> runCont (f a) k

--Longitud de una lista con la mónada CPS.
lengthcps :: [a] -> Continuation r Int 
lengthcps l@(x:xs) = if null l
                        then return 0
                        else do
                               m <- lengthcps xs;
                               return $m+1

--Toma elementos de una lista con la mónada CPS.
takecps :: Int -> [a] -> Continuation r [a] 
takecps n l@(x:xs) = case (n, l) of
                          (0, _)  -> return []
                          (_, []) -> return []
                          (n, l)  -> do
                                       m <- takecps (n-1) xs
                                       return $x:m

--Tira elementos de una lista con la mónada CPS.
dropcps :: Int -> [a] -> Continuation r [a] 
dropcps n l@(x:xs) = case (n, l) of
                          (0, _)  -> return [x]
                          (_, []) -> return []
                          (n, l)  -> do
                                       m <- dropcps (n-1) xs
                                       return m

--Genera se le asigne aleatoriamente de una lista de veinte platillos diferentes.
dieta :: IO [()]
dieta = do
          let dias = ["Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo"];
              platillos = ["Albóndigas", "Barbacoa", "Carnitas", "Chilaquiles", "Chiles Rellenos", "Ensalada", "Gorditas", "Pambazos", "Pancita", "Pechuga Empanizada", "Pescado", "Picadillo", "Pollo con Mole", "Pozole", "Quesadillas", "Sopes", "Tacos al Pastor", "Tamales", "Torta", "Tostadas de Pollo", "Verduras al Vapor"];
          plan <- forM dias (\dia -> do
                                      indice <- getStdRandom (randomR (0, 20))
                                      return (platillos !! indice))
          putStrLn "Organizador de comidas diarias :)\t";
          mapM putStrLn $ zipWith (\dia comida -> dia ++ ": " ++ comida ++ ".\t") dias plan

--Si el usuario ingresa una contraseña incorrecta muestre 
--el error del usuario en pantalla y vuelva a pedirle al 
--usuario una contraseña hasta que lo haga de manera correcta.
getPass2 :: IO ()
getPass2 = do 
             putStrLn "Ingresa un password (debe contener un número, una mayúscula, y tener mínimo 8 símbolos)";
             pass <- getLine;
             when (not $ any (\c -> elem c ['0'..'9']) pass)
                  (do putStrLn "La contraseña debe contener mínimo un número."; getPass2; exitSuccess);
             when (not $ any (\c -> elem c ['A'..'Z']) pass)
                  (do putStrLn "La contraseña debe contener una letra mayúscula."; getPass2; exitSuccess);
             when (length pass < 8)
                  (do putStrLn "La contraseña debe contener al menos 8 símbolos."; getPass2; exitSuccess);
             putStrLn "Contraseña Almacenada"

{-=============================================================-}
type ℝ = Double
data ℂ = Punto (ℝ,ℝ)

instance Show ℂ where
   show c = case c of
            Punto (r1,r2) -> "Punto("++ show r1 ++ ", " ++ show r2 ++")"

--El tipo ℂ forma parte de la clase Num.
--Basta con definir la suma y el producto.
instance Num ℂ where
   (+) (Punto (a,b)) (Punto (c,d)) = Punto (a+c, b+d)
   (*) (Punto (a,b)) (Punto (c,d)) = Punto ((a*c)+(b*d*(-1)),(b*c)+(d*a))

--Calcula la norma de un número complejo sin usar raı́z cuadrada.
norma :: ℂ -> ℝ
norma (Punto(a,b)) = raiz $a**2 + b**2

--Calcula la raiz de una manera diferente.
raiz :: (Floating a, Eq a) => a -> a
raiz x = raiz' x x 0
           where raiz' x r t = if t==r
                                 then r
                                 else raiz' x (0.5*((x/r)+r)) r 

--Recibe un número n y devuelve una cuadrı́cula con n^2 números
--complejos dentro del cuadro de lado 2 con centro en el origen.
plano :: ℝ -> [ℂ]
plano r = error "plano"

--Recibe una lista de números complejos y devuelve una 
--cadena como sigue: si c está en el conjunto de 
--Mandelbrot, pinta “1 ”; en otro caso, “0
mandelbrot_set :: [ℂ] -> String
mandelbrot_set c = error "mandelbrot_set"

--Hace lo mismo que la función anterior pero en paralelo.
mandelbrot_set_parallel :: [ℂ] -> String
mandelbrot_set_parallel c = error "mandelbrot_set_parallel"

main = error "main"