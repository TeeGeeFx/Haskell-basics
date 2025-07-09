-- HC4_AllTasks.hs
-- Tasks HC4T1 - HC4T8 using pattern matching and dedicated main functions

-- HC4T1 - Task 1: weatherReport using pattern matching
weatherReport :: String -> String
weatherReport "sunny"  = "It's a bright and beautiful day!"
weatherReport "rainy"  = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"
weatherReport _        = "Weather unknown"

main1 :: IO ()
main1 = do
    putStrLn "HC4T1 - Weather Report:"
    print $ weatherReport "sunny"
    print $ weatherReport "rainy"
    print $ weatherReport "snowy"

------------------------------------------------------------

-- HC4T2 - Task 2: dayType using pattern matching
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday"   = "It's a weekend!"
dayType day
    | day `elem` ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"] = "It's a weekday."
    | otherwise = "Invalid day"

main2 :: IO ()
main2 = do
    putStrLn "\nHC4T2 - Day Type:"
    print $ dayType "Saturday"
    print $ dayType "Monday"
    print $ dayType "Funday"

------------------------------------------------------------

-- HC4T3 - Task 3: gradeComment with pattern matching and guards
gradeComment :: Int -> String
gradeComment n
    | n >= 90 && n <= 100 = "Excellent!"
    | n >= 70 && n <= 89  = "Good job!"
    | n >= 50 && n <= 69  = "You passed."
    | n >= 0  && n <= 49  = "Better luck next time."
    | otherwise           = "Invalid grade"

main3 :: IO ()
main3 = do
    putStrLn "\nHC4T3 - Grade Comment:"
    print $ gradeComment 95
    print $ gradeComment 70
    print $ gradeComment 30
    print $ gradeComment 110

------------------------------------------------------------

-- HC4T4 - Task 4: Rewrite specialBirthday using pattern matching
specialBirthday :: Int -> String
specialBirthday 1  = "First birthday! Yay!"
specialBirthday 16 = "Sweet sixteen!"
specialBirthday 18 = "Adulthood begins!"
specialBirthday 21 = "Time to party!"
specialBirthday 50 = "Half a century old!"
specialBirthday _  = "Just another birthday."

main4 :: IO ()
main4 = do
    putStrLn "\nHC4T4 - Special Birthday:"
    print $ specialBirthday 1
    print $ specialBirthday 21
    print $ specialBirthday 35

------------------------------------------------------------

-- HC4T5 - Task 5: Add catch-all with age in message
specialBirthdayWithAge :: Int -> String
specialBirthdayWithAge 1  = "First birthday! Yay!"
specialBirthdayWithAge 16 = "Sweet sixteen!"
specialBirthdayWithAge 18 = "Adulthood begins!"
specialBirthdayWithAge 21 = "Time to party!"
specialBirthdayWithAge 50 = "Half a century old!"
specialBirthdayWithAge age = "You're " ++ show age ++ " years old today!"

main5 :: IO ()
main5 = do
    putStrLn "\nHC4T5 - Special Birthday with Age:"
    print $ specialBirthdayWithAge 1
    print $ specialBirthdayWithAge 35

------------------------------------------------------------

-- HC4T6 - Task 6: whatsInsideThisList using pattern matching
whatsInsideThisList :: [a] -> String
whatsInsideThisList []       = "The list is empty."
whatsInsideThisList [_]      = "The list has one element."
whatsInsideThisList [_, _]   = "The list has two elements."
whatsInsideThisList _        = "The list has many elements."

main6 :: IO ()
main6 = do
    putStrLn "\nHC4T6 - What's Inside This List:"
    print $ whatsInsideThisList ([] :: [Int])
    print $ whatsInsideThisList [1]
    print $ whatsInsideThisList [1, 2]
    print $ whatsInsideThisList [1, 2, 3]

------------------------------------------------------------

-- HC4T7 - Task 7: Return first and third elements
firstAndThird :: [a] -> (Maybe a, Maybe a)
firstAndThird (x:_:z:_) = (Just x, Just z)
firstAndThird (x:_:[])  = (Just x, Nothing)
firstAndThird (x:_)     = (Just x, Nothing)
firstAndThird []        = (Nothing, Nothing)

main7 :: IO ()
main7 = do
    putStrLn "\nHC4T7 - First and Third Elements:"
    print $ firstAndThird [10, 20, 30, 40]
    print $ firstAndThird [5, 6]
    print $ firstAndThird []

------------------------------------------------------------

-- HC4T8 - Task 8: Extract values from tuple
describeTuple :: (String, Int) -> String
describeTuple (name, age) = name ++ " is " ++ show age ++ " years old."

main8 :: IO ()
main8 = do
    putStrLn "\nHC4T8 - Describe Tuple:"
    print $ describeTuple ("Alice", 30)
    print $ describeTuple ("Bob", 45)

------------------------------------------------------------

-- Optional: Run all tasks
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
