-- HC7_AllTasks.hs
-- Tasks HC7T1 to HC7T10: Custom type classes, instances, constraints

import Text.Read (readMaybe)

-- HC7T1: Define Color with Eq instance
data Color = Red | Green | Blue deriving (Show, Read)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

main1 :: IO ()
main1 = do
    putStrLn "HC7T1 - Eq for Color:"
    print $ Red == Red    -- True
    print $ Red == Green  -- False

------------------------------------------------------------

-- HC7T2: Implement Ord for Color (Red < Green < Blue)
instance Ord Color where
    compare Red Green  = LT
    compare Red Blue   = LT
    compare Green Red  = GT
    compare Green Blue = LT
    compare Blue Red   = GT
    compare Blue Green = GT
    compare _ _        = EQ

main2 :: IO ()
main2 = do
    putStrLn "\nHC7T2 - Ord for Color:"
    print $ Red < Green     -- True
    print $ Blue > Red      -- True
    print $ Green <= Green  -- True

------------------------------------------------------------

-- HC7T3: Function with Eq and Ord
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

main3 :: IO ()
main3 = do
    putStrLn "\nHC7T3 - Compare Values:"
    print $ compareValues 5 10      -- 10
    print $ compareValues "a" "b"   -- "b"

------------------------------------------------------------

-- HC7T4: Shape with Show and Read
data Shape = Circle Double | Rectangle Double Double

instance Show Shape where
    show (Circle r)        = "Circle " ++ show r
    show (Rectangle w h)   = "Rectangle " ++ show w ++ " " ++ show h

instance Read Shape where
    readsPrec _ input =
        case words input of
            ["Circle", r] ->
                [(Circle (read r), "")]
            ["Rectangle", w, h] ->
                [(Rectangle (read w) (read h), "")]
            _ -> []

main4 :: IO ()
main4 = do
    putStrLn "\nHC7T4 - Show and Read Shape:"
    let c = Circle 5
        r = Rectangle 3 4
    print c
    print r
    print (read "Circle 5.0" :: Shape)

------------------------------------------------------------

-- HC7T5: squareArea with Num constraint
squareArea :: Num a => a -> a
squareArea side = side * side

main5 :: IO ()
main5 = do
    putStrLn "\nHC7T5 - Square Area:"
    print $ squareArea 4      -- 16
    print $ squareArea 3.5    -- 12.25

------------------------------------------------------------

-- HC7T6: circleCircumference with Integral and Floating
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * realToFrac r

main6 :: IO ()
main6 = do
    putStrLn "\nHC7T6 - Circle Circumference:"
    print $ circleCircumference (5 :: Int)
    print $ circleCircumference (2.5 :: Double)

------------------------------------------------------------

-- HC7T7: nextColor using Enum and Bounded
instance Enum Color where
    toEnum 0 = Red
    toEnum 1 = Green
    toEnum 2 = Blue
    toEnum _ = error "Invalid Color"

    fromEnum Red = 0
    fromEnum Green = 1
    fromEnum Blue = 2

instance Bounded Color where
    minBound = Red
    maxBound = Blue

nextColor :: Color -> Color
nextColor c
    | c == maxBound = minBound
    | otherwise = succ c

main7 :: IO ()
main7 = do
    putStrLn "\nHC7T7 - Next Color:"
    print $ nextColor Red     -- Green
    print $ nextColor Blue    -- Red

------------------------------------------------------------

-- HC7T8: Use Read to parse Shape
parseShape :: String -> Maybe Shape
parseShape str = readMaybe str :: Maybe Shape

main8 :: IO ()
main8 = do
    putStrLn "\nHC7T8 - Parse Shape:"
    print $ parseShape "Circle 3.0"
    print $ parseShape "Rectangle 4.0 5.0"
    print $ parseShape "Triangle 2.0 3.0"  -- Should be Nothing

------------------------------------------------------------

-- HC7T9: Describable type class
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "Yes"
    describe False = "No"

instance Describable Shape where
    describe (Circle r) = "A circle with radius " ++ show r
    describe (Rectangle w h) = "A rectangle with width " ++ show w ++ " and height " ++ show h

main9 :: IO ()
main9 = do
    putStrLn "\nHC7T9 - Describable Instances:"
    print $ describe True
    print $ describe (Rectangle 3 4)

------------------------------------------------------------

-- HC7T10: describeAndCompare with multiple constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y = describe (compareValues x y)

-- Example instance for test: define Ord for Shape by area
instance Eq Shape where
    (Circle r1) == (Circle r2) = r1 == r2
    (Rectangle w1 h1) == (Rectangle w2 h2) = w1 == w2 && h1 == h2
    _ == _ = False

instance Ord Shape where
    compare (Circle r1) (Circle r2) = compare (pi * r1^2) (pi * r2^2)
    compare (Rectangle w1 h1) (Rectangle w2 h2) = compare (w1 * h1) (w2 * h2)
    compare (Circle r) (Rectangle w h) = compare (pi * r^2) (w * h)
    compare (Rectangle w h) (Circle r) = compare (w * h) (pi * r^2)

main10 :: IO ()
main10 = do
    putStrLn "\nHC7T10 - Describe and Compare:"
    print $ describeAndCompare (Rectangle 3 4) (Circle 2)
    print $ describeAndCompare (Circle 5) (Circle 3)

------------------------------------------------------------

-- Run all tasks
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
