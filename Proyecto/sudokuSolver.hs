import Data.Array

-- Resuelve el ejemplo del rompecabezas especificado de la entrada.
main = do
    let solucion = resuelve tableroSudoku
    imprimeTablero solucion

-- Las valores en el tablero están representadas por enteros en el rango de 0..9 donde 0 representa "vacío".
type Valor = Int

-- Un cuadro es identificado por un par (fila, columna).
type Casilla = (Int, Int)

-- Un tablero Sudoku es una array 9x9 de valores.
type Tablero = Array Casilla Valor

-- El tablero Sudoku para ser resuelto.
tableroSudoku :: Tablero
tableroSudoku = array ((0, 0), (8, 8)) $ asociaSudoku ejemploSudoku

-- Rompecabezas de ejemplo http://en.wikipedia.org/wiki/Sudoku
ejemploSudoku :: [[Valor]]
ejemploSudoku = [[5, 3, 0,  0, 7, 0,  0, 0, 0],
                 [6, 0, 0,  1, 9, 5,  0, 0, 0],
                 [0, 9, 8,  0, 0, 0,  0, 6, 0],

                 [8, 0, 0,  0, 6, 0,  0, 0, 3],
                 [4, 0, 0,  8, 0, 3,  0, 0, 1],
                 [7, 0, 0,  0, 2, 0,  0, 0, 6],

                 [0, 6, 0,  0, 0, 0,  2, 8, 0],
                 [0, 0, 0,  4, 1, 9,  0, 0, 5],
                 [0, 0, 0,  0, 8, 0,  0, 7, 9]]

-- Regresa la primera solución o Nothing si no encuentra soluciones.
resuelve :: Tablero -> Maybe Tablero
resuelve = headOrNothing . soluciones

-- Regresa todas las soluciones.
soluciones :: Tablero -> [Tablero]
soluciones t = soluciones' (casillasVacias t) t
  where
    -- Dada una lista de casillas vacias en el tablero, escoge una casilla vacía,
    -- determina qué valor puede ser puesta en esa casilla, y entonces
    -- busca recursivamente todas las soluciones para ese grupo de valores.
    soluciones' :: [Casilla] -> Tablero -> [Tablero]
    soluciones' []     t = [t]
    soluciones' (x:xs) t = concatMap (soluciones' xs) candidatoTableros
      where
        candidatoValors  = [m | m <- [1..9], esValorPosible m x t]
        candidatoTableros = map (\m -> copiaConValor m x t) candidatoValors

-- Regresa una lista de casillas donde el valor es 0
casillasVacias :: Tablero -> [Casilla]
casillasVacias t = [(fil, col) | fil <- [0..8], col <- [0..8], t ! (fil, col) == 0]

-- Determina si el valor especificado se puede colocar en la posición especificada.
esValorPosible :: Valor -> Casilla -> Tablero -> Bool
esValorPosible m (fil, col) t = noEnFila && noEnColumna && noEnBloque
  where
    noEnFila    = notElem m $ valoresEnFila t fil
    noEnColumna = notElem m $ valoresEnColumna t col
    noEnBloque  = notElem m $ valoresEnBloque t (fil, col)

-- Regresa el tablero con el valor especificado en la Casilla especifica.
copiaConValor :: Valor -> Casilla -> Tablero -> Tablero
copiaConValor valor (fil, col) t = t // [((fil, col), valor)]

-- Regresa las valores en la fila especifada
valoresEnFila :: Tablero -> Int -> [Valor]
valoresEnFila t fil = [t ! loc | loc <- range((fil, 0), (fil, 8))]

-- Regresa las valores en la columna especificada
valoresEnColumna ::  Tablero -> Int -> [Valor]
valoresEnColumna t col = [t ! loc | loc <- range((0, col), (8, col))]

-- Regresa las valores en la bloque de 3x3 que incluye la Casilla especificada.
valoresEnBloque :: Tablero -> Casilla -> [Valor]
valoresEnBloque t (fil, col) = [t ! loc | loc <- casillas]
  where
    fil' = (div fil 3) * 3
    col' = (div col 3) * 3
    casillas = range((fil', col'), (fil' + 2, col' + 2))

-- Convierte una lista de filas de valores (como en ejemploSudoku) a una lista de asociaciones de array.
asociaSudoku :: [[Valor]] -> [(Casilla, Valor)]
asociaSudoku = concatMap asociaFil . zip [0..8]
  where
    asociaFil :: (Int, [Valor]) -> [((Int, Int), Valor)]
    asociaFil (fil, valores) = asociaCol fil $ zip [0..8] valores
    asociaCol :: Int -> [(Int, Valor)] -> [((Int, Int), Valor)]
    asociaCol fil cols = map (\(col, m) -> ((fil, col), m)) cols

-- Si no es vacía la lista, devuelve la cabeza. En otro caso, Nothing.
headOrNothing :: [a] -> Maybe a
headOrNothing []     = Nothing
headOrNothing (x:xs) = Just x

-- Imprimer el tablero en caso que no sea Nothing.
imprimeTablero :: Maybe Tablero -> IO ()
imprimeTablero Nothing  = putStrLn "No hay solución para el Sudoku que quieres resolver! (•̀o•́)ง"
imprimeTablero (Just t) = mapM_ putStrLn [show $ valoresEnFila t  fil | fil <- [0..8]]