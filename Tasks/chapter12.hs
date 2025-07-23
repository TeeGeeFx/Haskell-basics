-- HC12T1: Print a Welcome Message
welcomeMessage :: IO ()
welcomeMessage = putStrLn "Welcome to Haskell Programming!"

-- HC12T2: Add Two Numbers
addTwoNumbers :: IO ()
addTwoNumbers = do
  putStrLn "Enter first number:"
  a <- readLn
  putStrLn "Enter second number:"
  b <- readLn
  putStrLn $ "Sum: " ++ show (addTwo a b)

addTwo :: Int -> Int -> Int
addTwo x y = x + y

-- HC12T3: Factorial Function
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

runFactorial :: IO ()
runFactorial = do
  putStrLn "Enter a positive integer:"
  n <- readLn
  putStrLn $ "Factorial: " ++ show (factorial n)

-- HC12T4: First 10 Fibonacci Numbers
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

printFibonacci :: IO ()
printFibonacci = print $ map fibonacci [0..9]

-- HC12T5: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = clean == reverse clean
  where clean = map toLower $ filter (/= ' ') s

runPalindrome :: IO ()
runPalindrome = do
  putStrLn "Enter a word or phrase:"
  input <- getLine
  putStrLn $ if isPalindrome input
             then "It's a palindrome!"
             else "Not a palindrome."

-- HC12T6: Sort a List of Integers
sortList :: IO ()
sortList = do
  putStrLn "Enter integers separated by spaces:"
  input <- getLine
  let nums = map read $ words input :: [Int]
  putStrLn $ "Sorted: " ++ show (sort nums)

-- HC12T7: Calculate Circle Area
calculateCircleArea :: Double -> Double
calculateCircleArea r = pi * r * r

runCircleArea :: IO ()
runCircleArea = do
  putStrLn "Enter radius:"
  r <- readLn
  putStrLn $ "Area: " ++ show (calculateCircleArea r)

-- HC12T8: Merge Two Sorted Lists
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x < y     = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

runMergeLists :: IO ()
runMergeLists = do
  putStrLn "Enter sorted list 1 (space-separated):"
  l1 <- fmap (map read . words) getLine
  putStrLn "Enter sorted list 2 (space-separated):"
  l2 <- fmap (map read . words) getLine
  putStrLn $ "Merged: " ++ show (mergeLists l1 l2)

-- HC12T9: Read and Print File Content
readFileSafe :: FilePath -> IO ()
readFileSafe path = catch
  (readFile path >>= putStrLn)
  (\e -> do
    let err = show (e :: IOException)
    putStrLn $ "Error reading file: " ++ err)

runReadFile :: IO ()
runReadFile = do
  putStrLn "Enter file path:"
  path <- getLine
  readFileSafe path

-- HC12T10: Mathematical Operations Module Usage
runMathModule :: IO ()
runMathModule = do
  let a = 12
      b = 4
  putStrLn $ "Add: " ++ show (add a b)
  putStrLn $ "Subtract: " ++ show (subtract' a b)
  putStrLn $ "Multiply: " ++ show (multiply a b)
  putStrLn $ "Divide: " ++ show (divide a b)

-- === Main Menu ===
main :: IO ()
main = do
  putStrLn "\n=== Haskell Task Menu ==="
  putStrLn "1. Welcome Message"
  putStrLn "2. Add Two Numbers"
  putStrLn "3. Factorial"
  putStrLn "4. Fibonacci (First 10)"
  putStrLn "5. Palindrome Checker"
  putStrLn "6. Sort a List"
  putStrLn "7. Calculate Circle Area"
  putStrLn "8. Merge Two Sorted Lists"
  putStrLn "9. Read and Print File Content"
  putStrLn "10. Use MathOperations Module"
  putStrLn "Enter a task number:"
  choice <- getLine
  case choice of
    "1"  -> welcomeMessage
    "2"  -> addTwoNumbers
    "3"  -> runFactorial
    "4"  -> printFibonacci
    "5"  -> runPalindrome
    "6"  -> sortList
    "7"  -> runCircleArea
    "8"  -> runMergeLists
    "9"  -> runReadFile
    "10" -> runMathModule
    _    -> putStrLn "Invalid option."
