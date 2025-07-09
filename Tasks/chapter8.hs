-- HC8_AllTasks.hs
-- Tasks HC8T1 to HC8T10

------------------------------------------------------------
-- HC8T1: Type Synonyms and generateTx
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx fromAddr toAddr val = "From: " ++ fromAddr ++ ", To: " ++ toAddr ++ ", Value: " ++ show val

main1 :: IO ()
main1 = do
    putStrLn "HC8T1 - generateTx:"
    putStrLn $ generateTx "Alice123" "Bob456" 100

------------------------------------------------------------
-- HC8T2: PaymentMethod and Person type
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person
    { pname :: String
    , paddress :: (String, Int)
    , method :: PaymentMethod
    } deriving Show

bob :: Person
bob = Person "Bob" ("Main Street", 101) Cash

main2 :: IO ()
main2 = do
    putStrLn "\nHC8T2 - Person with PaymentMethod:"
    print bob

------------------------------------------------------------
-- HC8T3: Shape and area function
data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

main3 :: IO ()
main3 = do
    putStrLn "\nHC8T3 - Shape Area:"
    print $ area (Circle 5)       -- 78.54...
    print $ area (Rectangle 10 5) -- 50

------------------------------------------------------------
-- HC8T4: Employee using record syntax
data Employee = Employee
    { name :: String
    , experienceInYears :: Float
    } deriving Show

richard :: Employee
richard = Employee "Richard" 7.5

main4 :: IO ()
main4 = do
    putStrLn "\nHC8T4 - Employee:"
    print richard

------------------------------------------------------------
-- HC8T5: Person with name, age, and employment
data PersonStatus = PersonStatus
    { pname' :: String
    , age :: Int
    , isEmployed :: Bool
    } deriving Show

person1 :: PersonStatus
person1 = PersonStatus "Anna" 28 True

person2 :: PersonStatus
person2 = PersonStatus "Tom" 35 False

main5 :: IO ()
main5 = do
    putStrLn "\nHC8T5 - Persons:"
    print person1
    print person2

------------------------------------------------------------
-- HC8T6: Shape with different constructors using record syntax
data CircleShape = CircleShape
    { center :: (Float, Float)
    , color :: String
    , radius :: Float
    } deriving Show

data RectangleShape = RectangleShape
    { width :: Float
    , height :: Float
    , color' :: String
    } deriving Show

circle1 :: CircleShape
circle1 = CircleShape (0, 0) "Red" 10

rectangle1 :: RectangleShape
rectangle1 = RectangleShape 4 5 "Blue"

main6 :: IO ()
main6 = do
    putStrLn "\nHC8T6 - Shapes with Records:"
    print circle1
    print rectangle1

------------------------------------------------------------
-- HC8T7: Animal with constructors
data Animal = Dog String | Cat String

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "Dog named " ++ name
describeAnimal (Cat name) = "Cat named " ++ name

main7 :: IO ()
main7 = do
    putStrLn "\nHC8T7 - Describe Animals:"
    print $ describeAnimal (Dog "Buddy")
    print $ describeAnimal (Cat "Mittens")

------------------------------------------------------------
-- HC8T8: greet using type synonyms
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, " ++ n ++ "! You are " ++ show a ++ " years old."

main8 :: IO ()
main8 = do
    putStrLn "\nHC8T8 - Greet Function:"
    putStrLn $ greet "Eve" 30

------------------------------------------------------------
-- HC8T9: Transaction and createTransaction
data Transaction = Transaction
    { from :: Address
    , to :: Address
    , amount :: Value
    , transactionId :: String
    } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction f t v = transactionId (Transaction f t v ("tx-" ++ f ++ "-" ++ t ++ "-" ++ show v))

main9 :: IO ()
main9 = do
    putStrLn "\nHC8T9 - Transaction ID:"
    putStrLn $ createTransaction "A1" "B2" 250

------------------------------------------------------------
-- HC8T10: Book with deriving Show
data Book = Book
    { title :: String
    , author :: String
    , year :: Int
    } deriving Show

myBook :: Book
myBook = Book "Learn Haskell" "Jane Doe" 2025

main10 :: IO ()
main10 = do
    putStrLn "\nHC8T10 - Book:"
    print myBook

------------------------------------------------------------
-- Run all tests
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
