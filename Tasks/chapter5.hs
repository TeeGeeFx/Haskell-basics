-- HC5_AllTasks.hs
-- Tasks HC5T1 to HC5T10 with individual main functions

-- HC5T1: Apply a function three times
applyThrice :: (Int -> Int) -> Int -> Int
applyThrice f x = f (f (f x))

main1 :: IO ()
main1 = do
    putStrLn "HC5T1 - Apply Function Three Times:"
    print $ applyThrice (+1) 5      -- Should be 8
    print $ applyThrice (*2) 1      -- Should be 8

------------------------------------------------------------

-- HC5T2: Filter odd numbers from 1 to 30
filterOdds :: [Int]
filterOdds = filter odd [1..30]

main2 :: IO ()
main2 = do
    putStrLn "\nHC5T2 - Filtering Odd Numbers:"
    print filterOdds

------------------------------------------------------------

-- HC5T3: Check for any word starting with an uppercase letter
hasUppercaseWord :: [String] -> Bool
hasUppercaseWord = any (\word -> not (null word) && head word `elem` ['A'..'Z'])

main3 :: IO ()
main3 = do
    putStrLn "\nHC5T3 - Any Uppercase Word:"
    print $ hasUppercaseWord ["hello", "World", "haskell"]
    print $ hasUppercaseWord ["hello", "world"]

------------------------------------------------------------

-- HC5T4: Rewrite using lambda
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

main4 :: IO ()
main4 = do
    putStrLn "\nHC5T4 - Lambda for >10:"
    print $ biggerThan10 8
    print $ biggerThan10 15

------------------------------------------------------------

-- HC5T5: Partial application to multiply by 5
multiplyByFive :: Int -> Int
multiplyByFive = (*) 5

main5 :: IO ()
main5 = do
    putStrLn "\nHC5T5 - Multiply By Five (Partial Application):"
    print $ multiplyByFive 4
    print $ multiplyByFive 10

------------------------------------------------------------

-- HC5T6: Function composition to square and filter even numbers
squareEvens :: [Int] -> [Int]
squareEvens = filter even . map (^2)

main6 :: IO ()
main6 = do
    putStrLn "\nHC5T6 - Square Evens with Composition:"
    print $ squareEvens [1..10]  -- [4, 16, 36, 64, 100]

------------------------------------------------------------

-- HC5T7: Use $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

main7 :: IO ()
main7 = do
    putStrLn "\nHC5T7 - Using $ Operator:"
    print result

------------------------------------------------------------

-- HC5T8: Point-free version of addFive
addFive :: Int -> Int
addFive = (+5)

main8 :: IO ()
main8 = do
    putStrLn "\nHC5T8 - Point-Free addFive:"
    print $ addFive 3
    print $ addFive 10

------------------------------------------------------------

-- HC5T9: Apply a function twice to every list element
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

main9 :: IO ()
main9 = do
    putStrLn "\nHC5T9 - Transform List (Apply Function Twice):"
    print $ transformList (+1) [1, 2, 3]  -- [3, 4, 5]
    print $ transformList (*2) [1, 2]     -- [4, 8]

------------------------------------------------------------

-- HC5T10: Check if any squared value is > 50 using filter, map, any
anySquareGreaterThan50 :: [Int] -> Bool
anySquareGreaterThan50 = any (>50) . map (^2) . filter (>0)

main10 :: IO ()
main10 = do
    putStrLn "\nHC5T10 - Any Square > 50:"
    print $ anySquareGreaterThan50 [1, 5, 8]     -- True (64)
    print $ anySquareGreaterThan50 [1, 2, 3]     -- False

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
