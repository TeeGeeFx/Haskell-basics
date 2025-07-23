{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import System.Random (randomRIO)
import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.List (sort, group)

--------------------------------------------------------------------------------
-- HC14T5: Custom Data Type and Pattern Matching with @
--------------------------------------------------------------------------------

data Result a = Success a | Error String

handleResult :: Show a => Result a -> String
handleResult r@(Success val) = "Success with value: " ++ show val ++ " [" ++ show r ++ "]"
handleResult r@(Error msg)   = "Error occurred: " ++ msg ++ " [" ++ show r ++ "]"

instance Show a => Show (Result a) where
  show (Success x) = "Success " ++ show x
  show (Error e)   = "Error: " ++ e

--------------------------------------------------------------------------------
-- HC14T8 & HC14T9: Character Frequency Function (with PartialTypeSignatures)
--------------------------------------------------------------------------------

counts :: String -> _
counts =
  map (\cs -> (head cs, length cs)) .
  group .
  sort .
  map toLower .
  filter (/= ' ')

--------------------------------------------------------------------------------
-- HC14T10: Simple Test for `counts`
--------------------------------------------------------------------------------

testCounts :: IO ()
testCounts = do
  let input = "AaBbCc"
      expected = [('a',2),('b',2),('c',2)]
      actual = counts input
  if actual == expected
    then putStrLn "✅ Test passed: counts function works correctly."
    else putStrLn $ "❌ Test failed: Expected " ++ show expected ++ ", but got " ++ show actual

--------------------------------------------------------------------------------
-- HC14T1 – HC14T4, HC14T6, HC14T7: Main Program
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "== HC14 Cabal Tasks Combined =="

  -- HC14T1: Print a static message
  putStrLn "\nHC14T1: Hello, Cabal!"
  putStrLn "Hello, Cabal!"

  -- HC14T2: Generate a random number
  putStrLn "\nHC14T2: Generating a random number between 1 and 100..."
  num <- randomRIO (1, 100)
  putStrLn $ "Random number: " ++ show num

  -- HC14T3: NumericUnderscores extension
  putStrLn "\nHC14T3: Demonstrating NumericUnderscores"
  let population = 1_000_000
      budget     = 250_000
  putStrLn $ "Population: " ++ show population
  putStrLn $ "Budget: " ++ show budget

  -- HC14T4: TypeApplications with `read`
  putStrLn "\nHC14T4: Reading an Int using TypeApplications"
  case readMaybe @Int "42" of
    Just n  -> putStrLn $ "Parsed number: " ++ show n
    Nothing -> putStrLn "Failed to parse number."

  -- HC14T5: Pattern Matching with @
  putStrLn "\nHC14T5: Pattern matching with @ on custom Result type"
  putStrLn $ handleResult (Success 99)
  putStrLn $ handleResult (Error "Something failed.")

  -- HC14T8: Character frequency counter
  putStrLn "\nHC14T8: Character frequency in 'Haskell is Awesome'"
  let freq = counts "Haskell is Awesome"
  print freq

  -- HC14T10: Test suite for counts
  putStrLn "\nHC14T10: Running test suite for counts"
  testCounts
