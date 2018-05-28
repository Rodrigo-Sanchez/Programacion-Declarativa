module Arreglos where

--Un arreglo es una función de los enteros en un tipo 'a'; la segunda componente es para conocer su longitud.
data Arreglo a = Arr (Int->a) Int


--Para pintar los arreglos de forma bonita
instance (Show a) => Show (Arreglo a) where
      show (Arr f n) =  "["++ pinta 0 n f where 
                              pinta i n f | n==0 = "]" 
                                          | i==0 = show (f i)++pinta (i+1) n f 
                                          | i==n = "]"
                                          | otherwise = ","++show (f i)++pinta (i+1) n f

--Ejemplo de un arreglo
arr = Arr f 11 where 
          f n = case n of
                 0 -> 2
                 1 -> 1
                 2 -> 23
                 3 -> 3
                 4 -> 0
                 5 -> 4
                 6 -> 10
                 7 -> 11
                 8 -> 22
                 9 -> 9
                 10 -> 50
                 _ -> error "fuera de índice"

--Ejemplo de un arreglo
arr_alg1 = Arr f 10 where 
          f n = case n of
                 0 -> 100
                 1 -> 70
                 2 -> 80
                 3 -> 60
                 4 -> 90
                 5 -> 20
                 6 -> 10
                 7 -> 17
                 8 -> 2
                 9 -> 85
                 _ -> error "fuera de índice"

--Ejemplo de un arreglo
arr1 = Arr f 4 where 
          f n = case n of
                 0 -> 21
                 1 -> 22
                 2 -> 24
                 3 -> 27
                 _ -> error "fuera de índice"

arr_colores = Arr f 21 where 
                     f n = case n of 
                         0 -> "azul"
                         1 -> "azul"
                         2 -> "verde"
                         3 -> "verde"
                         4 -> "verde"
                         5 -> "verde"
                         6 -> "rojo"
                         7 -> "rojo"
                         8 -> "azul"
                         9 -> "rojo"
                         10 -> "rojo"
                         11 -> "verde"
                         12 -> "verde"
                         13 -> "rojo"
                         14 -> "rojo"
                         15 -> "azul"
                         16 -> "rojo"
                         17 -> "azul"
                         18 -> "azul"
                         19 -> "verde"
                         20 -> "azul"
                         _ -> error "fuera de indice"

--Para obtener el elemento en la i-ésima posición de un arreglo
get::Arreglo a->Int->a
get (Arr f n) i | i>=0 && i < n = f i
                | otherwise = error ("no :v "  ++ (show i))

--Para sobreescribir el elemento en la i-ésima posición
upd::Arreglo a->Int->a->Arreglo a
upd (Arr f n) i x = Arr (\m -> if m==i then x else f m) n

--Para cambiar el tamaño de un arreglo
setSize :: Arreglo a -> Int -> Arreglo a
setSize (Arr f _) i = (Arr f i)

--Para obtener el tamaño de un arreglo
size::Arreglo a->Int
size (Arr _ n) = n

--Para saber si un elemento pertenece a un arreglo
elemArr::Eq a=>a->Arreglo a->Int
elemArr x arr = busca x 0 arr where
                busca x i arr | i < size arr = if x == get arr i then i else busca x (i+1) arr      
                              | otherwise = -1


--Para obtener la posición del mínimo en un arreglo que está en la posición 'i' en adelante.
minArr::Ord a=>Arreglo a->Int->Int
minArr arr i = buscaMin i (i+1) arr where
               buscaMin m_i j arr | j==size arr = m_i
                                  | otherwise = if get arr m_i < get arr j then buscaMin m_i (j+1) arr else buscaMin j (j+1) arr  

--Intercambia los elementos en la posición 'i' y 'j' de un arreglo.
swap::Arreglo a->Int->Int->Arreglo a
swap arr i j = let xi = get arr i
                   xj = get arr j
                   f' n | n == i = xj
                        | n == j = xi
                        | otherwise = get arr n in
               Arr f' (size arr)              
                              
                                
--Ordena un arreglo con selectionSort
selectionSort::Ord a=>Arreglo a->Arreglo a
selectionSort arr = ordena 0 arr where
                    ordena i arr | i<size arr = let m = minArr arr i 
                                                    arr' = swap arr i m in 
                                                ordena (i+1) arr'              
                                 | otherwise = arr    



bubbleSort::Ord a=>Arreglo a->Arreglo a
bubbleSort arr = ordena arr 0 where
                 ordena arr i | i == size arr-1 = arr 
                              | otherwise = ordena (bubble arr i 0) (i+1) 
                 bubble arr i j | j == (size arr)-i-1 = arr 
                                | otherwise = if get arr j > get arr (j+1)  
                                              then bubble (swap arr j (j+1)) i (j+1) 
                                              else bubble arr i (j+1)                        


insertionSort::Ord a=>[a]->[a]
insertionSort xs = case xs of
                    [] -> []
                    x:xs -> inserta x $ insertionSort xs where
                            inserta x [] = [x]
                            inserta x (y:ys) = if x<y then (x:(y:ys)) 
                                                      else (y:(inserta x ys))  
