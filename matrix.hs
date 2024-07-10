matrizA = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
matrizB = [[9, 8, 7], [6, 5, 4], [3, 2, 1]]

type Matrix a = [[a]]

printMatrix :: Show a => Matrix a -> IO ()
printMatrix mat = mapM_ printRow mat
  where
    printRow row = putStrLn $ unwords $ map show row

addMatrices :: Num a => Matrix a -> Matrix a -> Matrix a
addMatrices mat1 mat2 = zipWith (zipWith (+)) mat1 mat2

multiplyMatrices :: Num a => Matrix a -> Matrix a -> Matrix a
multiplyMatrices mat1 mat2 =
  [[sum $ zipWith (*) row col | col <- transposeMatrix mat1] | row <- mat2]

determinant :: Num a => Matrix a -> a
determinant [[x]] = x
determinant mat = sum [(-1) ^ i * head mat !! i * determinant (minor 0 i mat) | i <- [0 .. n - 1]]
  where
    n = length mat
    minor x y mat' = map (deleteAt y) (deleteAt x mat')
    deleteAt i xs = take i xs ++ drop (i + 1) xs

inverseMatrix :: Fractional a => Matrix a -> Matrix a
inverseMatrix mat = transposeMatrix(map (map (/ det)) (transposeMatrix cofactorMatrix))
  where
    det = determinant mat
    cofactorMatrix = [[(-1) ^ (i + j) * determinant (minor i j mat) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]
    n = length mat
    minor x y mat' = map (deleteAt y) (deleteAt x mat')
    deleteAt i xs = take i xs ++ drop (i + 1) xs

matriz_float :: Matrix Integer -> Matrix Double
matriz_float mat = map (map fromIntegral) mat

transposeMatrix :: Matrix a -> Matrix a
transposeMatrix mat = foldr (zipWith (:)) (repeat []) mat

isSymmetric :: Eq a => Matrix a -> Bool
isSymmetric mat = mat == transposeMatrix mat

scalarMultiply :: Num a => a -> Matrix a -> Matrix a
scalarMultiply k = map (map (* k))

getRow :: Int -> Matrix a -> [a]
getRow i mat = mat !! i

getColumn :: Int -> Matrix a -> [a]
getColumn j mat = map (!! j) mat


