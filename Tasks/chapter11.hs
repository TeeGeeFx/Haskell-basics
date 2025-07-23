-- HC11T1: Greet the User
greetUser :: IO ()
greetUser = do
  putStrLn "Enter your name:"
  name <- getLine
  putStrLn $ "Hello, " ++ name ++ "!"

-- HC11T2: Count Characters in a Line
countCharacters :: IO ()
countCharacters = do
  putStrLn "Enter a line:"
  line <- getLine
  putStrLn $ "Character count: " ++ show (length line)

-- HC11T3: Double a Number
doubleNumber :: IO ()
doubleNumber = do
  putStrLn "Enter a number:"
  input <- getLine
  let num = read input :: Int
  putStrLn $ "Double: " ++ show (num * 2)

-- HC11T4: Concatenate Two Lines
concatenateLines :: IO ()
concatenateLines = do
  putStrLn "Enter first line:"
  line1 <- getLine
  putStrLn "Enter second line:"
  line2 <- getLine
  putStrLn $ "Concatenated: " ++ (line1 ++ line2)

-- HC11T5: Repeat Until "quit"
repeatUntilQuit :: IO ()
repeatUntilQuit = do
  putStrLn "Type something (type \"quit\" to stop):"
  line <- getLine
  when (line /= "quit") $ do
    putStrLn $ "You typed: " ++ line
    repeatUntilQuit

-- HC11T6: Uppercase Converter
toUppercase :: IO ()
toUppercase = do
  putStrLn "Enter a line to convert to uppercase:"
  line <- getLine
  putStrLn $ map toUpper line

-- HC11T7: User Options
userOptions :: IO ()
userOptions = do
  putStrLn "Choose an option:"
  putStrLn "1. Say Hello"
  putStrLn "2. Say Goodbye"
  putStrLn "3. Exit"
  option <- getLine
  case option of
    "1" -> putStrLn "Hello!"
    "2" -> putStrLn "Goodbye!"
    "3" -> putStrLn "Exiting..."
    _   -> putStrLn "Invalid option."

-- HC11T8: Even or Odd Checker
evenOrOdd :: IO ()
evenOrOdd = do
  putStrLn "Enter a number:"
  input <- getLine
  let num = read input :: Int
  putStrLn $ if even num then "Even" else "Odd"

-- HC11T9: Sum Two Numbers
sumTwoNumbers :: IO ()
sumTwoNumbers = do
  putStrLn "Enter first number:"
  input1 <- getLine
  putStrLn "Enter second number:"
  input2 <- getLine
  let num1 = read input1 :: Int
      num2 = read input2 :: Int
  putStrLn $ "Sum: " ++ show (num1 + num2)

-- HC11T10: Reverse User Input
reverseInput :: IO ()
reverseInput = do
  putStrLn "Enter text to reverse:"
  input <- getLine
  putStrLn $ "Reversed: " ++ reverse input

-- Main function with a menu to select a task
main :: IO ()
main = do
  putStrLn "Select a task to run (1-10):"
  putStrLn "1. Greet the User"
  putStrLn "2. Count Characters in a Line"
  putStrLn "3. Double a Number"
  putStrLn "4. Concatenate Two Lines"
  putStrLn "5. Repeat Until \"quit\""
  putStrLn "6. Uppercase Converter"
  putStrLn "7. User Options"
  putStrLn "8. Even or Odd Checker"
  putStrLn "9. Sum Two Numbers"
  putStrLn "10. Reverse User Input"
  task <- getLine
  case task of
    "1"  -> greetUser
    "2"  -> countCharacters
    "3"  -> doubleNumber
    "4"  -> concatenateLines
    "5"  -> repeatUntilQuit
    "6"  -> toUppercase
    "7"  -> userOptions
    "8"  -> evenOrOdd
    "9"  -> sumTwoNumbers
    "10" -> reverseInput
    _    -> putStrLn "Invalid selection."
