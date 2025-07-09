-- HC6_AllTasks.hs
-- Tasks HC6T1 to HC6T10 using recursion and folds

-- HC6T1: Factorial (recursive)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main1 :: IO ()
main1 = do
    putStrLn "HC6T1 - Factorial:"
    print $ factorial 5   -- 120
    print $ factorial 0   -- 1

------------------------------------------------------------

-- HC6T2: Fibonacci (recursive)
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

main2 :: IO ()
main2 = do
    putStrLn "\nHC6T2 - Fibonacci:"
    print $ fibonacci 0   -- 0
    print $ fibonacci 1   -- 1
    print $ fibonacci 7   -- 13

------------------------------------------------------------

-- HC6T3: Sum of list using foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

main3 :: IO ()
main3 = do
    putStrLn "\nHC6T3 - Sum using foldr:"
    print $ sumList [1..5]  -- 15

------------------------------------------------------------

-- HC6T4: Product of list using foldl
productList :: [Int] -> Int
productList = foldl (*) 1

main4 :: IO ()
main4 = do
    putStrLn "\nHC6T4 - Product using foldl:"
    print $ productList [1..4]  -- 24

------------------------------------------------------------

-- HC6T5: Reverse list using recursion
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

main5 :: IO ()
main5 = do
    putStrLn "\nHC6T5 - Reverse List:"
    print $ reverseList [1, 2, 3, 4]  -- [4,3,2,1]

------------------------------------------------------------

-- HC6T6: Element exists in list
elementExists :: Eq a => a -> [a] -> Bool
elementExists _ [] = False
elementExists x (y:ys) = x == y || elementExists x ys

main6 :: IO ()
main6 = do
    putStrLn "\nHC6T6 - Element Exists:"
    print $ elementExists 3 [1,2,3,4]  -- True
    print $ elementExists 9 [1,2,3,4]  -- False

------------------------------------------------------------

-- HC6T7: Length of list
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

main7 :: IO ()
main7 = do
    putStrLn "\nHC6T7 - List Length:"
    print $ listLength [1..10]  -- 10

------------------------------------------------------------

-- HC6T8: Filter even numbers
filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)
    | even x    = x : filterEvens xs
    | otherwise = filterEvens xs

main8 :: IO ()
main8 = do
    putStrLn "\nHC6T8 - Filter Evens:"
    print $ filterEvens [1..10]  -- [2,4,6,8,10]

------------------------------------------------------------

-- HC6T9: Map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

main9 :: IO ()
main9 = do
    putStrLn "\nHC6T9 - Custom Map:"
    print $ myMap (+1) [1,2,3]  -- [2,3,4]

------------------------------------------------------------

-- HC6T10: Get digits of number recursively
digits :: Int -> [Int]
digits n
    | n < 10 = [n]
    | otherwise = digits (n `div` 10) ++ [n `mod` 10]

main10 :: IO ()
main10 = do
    putStrLn "\nHC6T10 - Digits of a Number:"
    print $ digits 12345  -- [1,2,3,4,5]

------------------------------------------------------------

-- Optional: Run all
main :: IO ()
main = do
    main1
    main2
    main3
    main4
    main5
    main6
    main7
    main8
    main9
    main10
