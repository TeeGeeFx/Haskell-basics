import Data.Char (toUpper)
import Data.List (nub, sort, group)
import qualified Data.Map as Map

-- HC16T1: Reverse a String
reverseString :: String -> String
reverseString = reverse

-- HC16T2: Palindrome Checker
isPalindrome :: String -> Bool
isPalindrome s = let cleaned = map toLower (filter (/= ' ') s)
                 in cleaned == reverse cleaned

-- HC16T3: Factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- HC16T4: Filter Even Numbers
filterEven :: [Int] -> [Int]
filterEven = filter even

-- HC16T5: Uppercase String
toUppercase :: String -> String
toUppercase = map toUpper

-- HC16T6: nth Fibonacci Number
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- HC16T7: Element Existence in List
elementExists :: Eq a => a -> [a] -> Bool
elementExists = elem

-- HC16T8: Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

-- HC16T9: Remove Duplicates from List
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

-- HC16T10: Character Frequency in String
charFrequency :: String -> [(Char, Int)]
charFrequency s = map (\cs -> (head cs, length cs)) . group . sort $ filter (/= ' ') s

-- Main Function Demonstration
main :: IO ()
main = do
  putStrLn "== HC16 Haskell Tasks =="

  putStrLn "\nReversed String:"
  print $ reverseString "haskell"

  putStrLn "\nPalindrome Check:"
  print $ isPalindrome "racecar"
  print $ isPalindrome "hello"

  putStrLn "\nFactorial of 5:"
  print $ factorial 5

  putStrLn "\nEven Numbers from List:"
  print $ filterEven [1..10]

  putStrLn "\nUppercase Conversion:"
  print $ toUppercase "functional programming"

  putStrLn "\nnth Fibonacci Number (10):"
  print $ fibonacci 10

  putStrLn "\nCheck if 3 exists in list [1,2,3,4]:"
  print $ elementExists 3 [1,2,3,4]

  putStrLn "\nInsertion Sort:"
  print $ insertionSort [4,2,5,1,3]

  putStrLn "\nRemove Duplicates:"
  print $ removeDuplicates [1,2,2,3,4,4,5]

  putStrLn "\nCharacter Frequency in 'hello world':"
  print $ charFrequency "hello world"
