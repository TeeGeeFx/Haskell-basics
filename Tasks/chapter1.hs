-- HC1_AllTasks.hs
-- Tasks HC1T1 - HC1T8 with separate main function per task

import Data.List (sortBy)

-- HC1T1 - Task 1: Function Composition
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double

main1 :: IO ()
main1 = do
    putStrLn "HC1T1 - Function Composition:"
    print $ double 4             -- 8
    print $ increment 8          -- 9
    print $ doubleThenIncrement 4  -- 9


-- HC1T2 - Task 2: Pure Function Example
circleArea :: Floating a => a -> a
circleArea r = pi * r * r

main2 :: IO ()
main2 = do
    putStrLn "\nHC1T2 - Pure Function Example:"
    print $ circleArea 3         -- 28.274333882308138


-- HC1T3 - Task 3: Greater Than 18
greaterThan18 :: Int -> Bool
greaterThan18 x = x > 18

main3 :: IO ()
main3 = do
    putStrLn "\nHC1T3 - Greater Than 18:"
    print $ greaterThan18 20     -- True
    print $ greaterThan18 10     -- False


-- HC1T4 - Task 4: Composing Player Functions
type Player = (String, Int)

extractPlayers :: [Player] -> [String]
extractPlayers = map fst

sortByScore :: [Player] -> [Player]
sortByScore = reverse . sortBy (\(_, s1) (_, s2) -> compare s1 s2)

topThree :: [Player] -> [Player]
topThree = take 3

getTopThreePlayers :: [Player] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

main4 :: IO ()
main4 = do
    putStrLn "\nHC1T4 - Player Data Processing:"
    let players = [("Alice", 90), ("Bob", 70), ("Charlie", 85), ("Dave", 95)]
    print $ getTopThreePlayers players  -- ["Dave","Alice","Charlie"]


-- HC1T5 - Task 5: Laziness in Haskell
infiniteNumbers :: [Int]
infiniteNumbers = [1..]

firstN :: Int -> [Int]
firstN n = take n infiniteNumbers

main5 :: IO ()
main5 = do
    putStrLn "\nHC1T5 - Laziness in Haskell:"
    print $ firstN 10  -- [1,2,3,4,5,6,7,8,9,10]


-- HC1T6 - Task 6: Type Signature
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

main6 :: IO ()
main6 = do
    putStrLn "\nHC1T6 - Type Signature:"
    print $ addNumbers 3 5  -- 8


-- HC1T7 - Task 7: Fahrenheit to Celsius
fToC :: Fractional a => a -> a
fToC f = (f - 32) * 5 / 9

main7 :: IO ()
main7 = do
    putStrLn "\nHC1T7 - Fahrenheit to Celsius:"
    print $ fToC 98.6  -- 37.0


-- HC1T8 - Task 8: Higher-Order Function
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

main8 :: IO ()
main8 = do
    putStrLn "\nHC1T8 - Higher-Order Function:"
    print $ applyTwice (+2) 5     -- 9
    print $ applyTwice (*3) 2     -- 18


-- Optional: Uncomment to run all tests together
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
