```
-- Funkcja sumList, która sumuje wszystkie liczby w liście
sumList :: Num a => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Przykład użycia funkcji sumList
main :: IO ()
main = do
    let numbers = [1, 2, 3, 4, 5]
    let result = sumList numbers
    print result
```
```
-- Funkcja bubbleSort, która sortuje listę liczb
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort' xs (length xs)

-- Funkcja pomocnicza do sortowania
bubbleSort' :: (Ord a) => [a] -> Int -> [a]
bubbleSort' xs 0 = xs
bubbleSort' xs n = bubbleSort' (bubblePass xs) (n - 1)

-- Funkcja wykonująca jedno przejście sortowania bąbelkowego
bubblePass :: (Ord a) => [a] -> [a]
bubblePass (x:y:xs)
  | x > y     = y : bubblePass (x:xs)
  | otherwise = x : bubblePass (y:xs)
bubblePass xs = xs

-- Przykład użycia funkcji bubbleSort
main :: IO ()
main = do
    let numbers = [64, 34, 25, 12, 22, 11, 90]
    let sortedNumbers = bubbleSort numbers
    print sortedNumbers
```

```
-- Funkcja obliczająca największy wspólny dzielnik (NWD)
nwd :: Integer -> Integer -> Integer
nwd a 0 = abs a
nwd a b = nwd b (a `mod` b)

-- Funkcja obliczająca najmniejszą wspólną wielokrotność (NWW)
nww :: Integer -> Integer -> Integer
nww a b = abs (a * b) `div` (nwd a b)

-- Przykłady użycia funkcji nwd i nww
main :: IO ()
main = do
    let a = 56
    let b = 98
    let resultNWD = nwd a b
    putStrLn $ "NWD(" ++ show a ++ ", " ++ show b ++ ") = " ++ show resultNWD

    let resultNWW = nww a b
    putStrLn $ "NWW(" ++ show a ++ ", " ++ show b ++ ") = " ++ show resultNWW
```
```-- Funkcja generująca wariacje z powtórzeniami
variationsWithRepetition :: Int -> [a] -> [[a]]
variationsWithRepetition 0 _ = [[]]
variationsWithRepetition n xs = [ x:ys | x <- xs, ys <- variationsWithRepetition (n-1) xs ]

-- Funkcja generująca wariacje bez powtórzeń
variationsWithoutRepetition :: Int -> [a] -> [[a]]
variationsWithoutRepetition 0 _ = [[]]
variationsWithoutRepetition n xs = [ x:ys | (x, rest) <- selections xs, ys <- variationsWithoutRepetition (n-1) rest ]

-- Funkcja pomocnicza do generowania wszystkich możliwych wyborów z listy
selections :: [a] -> [(a, [a])]
selections [] = []
selections (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- selections xs]

-- Przykłady użycia funkcji
main :: IO ()
main = do
    -- Wariacje z powtórzeniami
    let elements = ['a', 'b', 'c']
    let k = 2
    let resultWithRepetition = variationsWithRepetition k elements
    putStrLn "Wariacje z powtórzeniami:"
    print resultWithRepetition

    -- Wariacje bez powtórzeń
    let resultWithoutRepetition = variationsWithoutRepetition k elements
    putStrLn "Wariacje bez powtórzeń:"
    print resultWithoutRepetition
```
