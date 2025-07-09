-- HC3_AllTasks.hs
-- Tasks HC3T1 to HC3T10 with individual main functions

import Text.Printf (printf)

-- HC3T1 - Task 1: Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber n = if n > 0 then "Positive"
                else if n < 0 then "Negative"
                else "Zero"

main1 :: IO ()
main1 = do
    putStrLn "HC3T1 - Check Number:"
    print $ checkNumber 5
    print $ checkNumber (-3)
    print $ checkNumber 0

------------------------------------------------------------

-- HC3T2 - Task 2: Grade classification using guards
grade :: Int -> String
grade score
    | score >= 90 = "A"
    | score >= 80 = "B"
    | score >= 70 = "C"
    | score >= 60 = "D"
    | otherwise   = "F"

main2 :: IO ()
main2 = do
    putStrLn "\nHC3T2 - Grade:"
    print $ grade 95
    print $ grade 72
    print $ grade 50

------------------------------------------------------------

-- HC3T3 - Task 3: RGB to Hex string using let bindings
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) =
    let rHex = printf "%02X" r
        gHex = printf "%02X" g
        bHex = printf "%02X" b
    in rHex ++ gHex ++ bHex

main3 :: IO ()
main3 = do
    putStrLn "\nHC3T3 - RGB to Hex:"
    print $ rgbToHex (255, 0, 127)
    print $ rgbToHex (0, 255, 64)

------------------------------------------------------------

-- HC3T4 - Task 4: Triangle area using Heron’s formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

main4 :: IO ()
main4 = do
    putStrLn "\nHC3T4 - Triangle Area:"
    print $ triangleArea 3 4 5
    print $ triangleArea 7 8 9

------------------------------------------------------------

-- HC3T5 - Task 5: Triangle type using guards
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

main5 :: IO ()
main5 = do
    putStrLn "\nHC3T5 - Triangle Type:"
    print $ triangleType 3 3 3
    print $ triangleType 5 5 8
    print $ triangleType 6 7 8

------------------------------------------------------------

-- HC3T6 - Task 6: Check for leap year using if-then-else
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

main6 :: IO ()
main6 = do
    putStrLn "\nHC3T6 - Leap Year Check:"
    print $ isLeapYear 2000
    print $ isLeapYear 1900
    print $ isLeapYear 2024

------------------------------------------------------------

-- HC3T7 - Task 7: Determine season based on month using guards
season :: Int -> String
season month
    | month == 12 || month == 1 || month == 2 = "Winter"
    | month >= 3 && month <= 5 = "Spring"
    | month >= 6 && month <= 8 = "Summer"
    | month >= 9 && month <= 11 = "Autumn"
    | otherwise = "Invalid month"

main7 :: IO ()
main7 = do
    putStrLn "\nHC3T7 - Season:"
    print $ season 3
    print $ season 7
    print $ season 11

------------------------------------------------------------

-- HC3T8 - Task 8: BMI Category using where
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25 = "Normal"
    | bmi < 30 = "Overweight"
    | otherwise = "Obese"
    where bmi = weight / (height ^ 2)

main8 :: IO ()
main8 = do
    putStrLn "\nHC3T8 - BMI Category:"
    print $ bmiCategory 70 1.75
    print $ bmiCategory 90 1.8

------------------------------------------------------------

-- HC3T9 - Task 9: Max of three using let
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let maxAB = max a b
        maxABC = max maxAB c
    in maxABC

main9 :: IO ()
main9 = do
    putStrLn "\nHC3T9 - Max of Three:"
    print $ maxOfThree 10 20 15
    print $ maxOfThree 5 25 10

------------------------------------------------------------

-- HC3T10 - Task 10: Palindrome using recursion and guards
isPalindrome :: String -> Bool
isPalindrome s
    | length s <= 1 = True
    | head s == last s = isPalindrome (init (tail s))
    | otherwise = False

main10 :: IO ()
main10 = do
    putStrLn "\nHC3T10 - Palindrome Check:"
    print $ isPalindrome "racecar"
    print $ isPalindrome "haskell"
    print $ isPalindrome "madam"

------------------------------------------------------------

-- Optional main to run all at once
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
