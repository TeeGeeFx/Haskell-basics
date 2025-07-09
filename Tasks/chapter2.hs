-- HC2_AllTasks.hs
-- Tasks HC2T1 - HC2T7 with separate main function for each task

import Data.Int (Int64)

-- HC2T1 - Task 1: Checking Types in GHCi
-- Expected types:
-- 42              :: Integer or Num a => a
-- 3.14            :: Fractional a => a
-- "Haskell"       :: [Char] or String
-- 'Z'             :: Char
-- True && False   :: Bool

main1 :: IO ()
main1 = do
    putStrLn "HC2T1 - Checking Types (Expected Results):"
    print (42 :: Int)
    print (3.14 :: Double)
    print ("Haskell")
    print ('Z')
    print (True && False)

------------------------------------------------------------

-- HC2T2 - Task 2: Function Type Signatures and Implementations

add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

main2 :: IO ()
main2 = do
    putStrLn "\nHC2T2 - Function Type Signatures:"
    print $ add 3 4
    print $ isEven 6
    print $ isEven 7
    print $ concatStrings "Hello, " "World!"

------------------------------------------------------------

-- HC2T3 - Task 3: Immutable Variables

myAge :: Int
myAge = 25

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- Attempting to redefine `myAge = 30` would cause a compilation error
-- because Haskell variables are immutable

main3 :: IO ()
main3 = do
    putStrLn "\nHC2T3 - Immutable Variables:"
    print myAge
    print piValue
    print greeting
    print isHaskellFun

------------------------------------------------------------

-- HC2T4 - Task 4: Infix and Prefix Notation

main4 :: IO ()
main4 = do
    putStrLn "\nHC2T4 - Infix and Prefix Notation:"
    -- Infix to prefix
    print $ (+) 5 3
    print $ (*) 10 4
    print $ (&&) True False

    -- Prefix to infix
    print $ 7 + 2
    print $ 6 * 5
    print $ True && False

------------------------------------------------------------

-- HC2T5 - Task 5: Defining and Using Functions

circleArea :: Float -> Float
circleArea r = pi * r * r

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max x (max y z)

main5 :: IO ()
main5 = do
    putStrLn "\nHC2T5 - Defining and Using Functions:"
    print $ circleArea 5.0
    print $ maxOfThree 10 20 15
    print $ maxOfThree 3 2 1

------------------------------------------------------------

-- HC2T6 - Task 6: Int vs Integer

smallNumber :: Int
smallNumber = 2 ^ 62

bigNumber :: Integer
bigNumber = 2 ^ 127

-- Trying to evaluate 2^64 :: Int may cause overflow depending on system
-- On 64-bit systems it may overflow; on 32-bit it definitely will

main6 :: IO ()
main6 = do
    putStrLn "\nHC2T6 - Int vs Integer:"
    print smallNumber
    print bigNumber
    -- Uncomment to test in GHCi manually:
    -- print (2^64 :: Int)

------------------------------------------------------------

-- HC2T7 - Task 7: Boolean Expressions

main7 :: IO ()
main7 = do
    putStrLn "\nHC2T7 - Boolean Expressions:"
    print $ True && True         -- True
    print $ False || False       -- False
    print $ not False            -- True
    print $ 10 > 20              -- False

------------------------------------------------------------

-- Optional: Run all tasks together
main :: IO ()
main = do
    main1
    main2
    main3
    main4
    main5
    main6
    main7
