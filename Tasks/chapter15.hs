module Main where

import System.IO
import System.IO.Error
import Control.Exception
import Data.Typeable
import Text.Read (readMaybe)
import Data.Char (toLower)

-- HC15T1: Handle Exceptions for File Reading and Velocity Calculation

velocityFromFile :: FilePath -> IO ()
velocityFromFile path = do
  eContent <- readFileSafe path
  case eContent of
    Left err -> putStrLn $ "File error: " ++ show err
    Right content -> do
      let [dStr, tStr] = words content ++ repeat "0"
      calculateVelocityHybrid dStr tStr
-- HC15T2: Self-Driving AI Car System

reactToTrafficLight :: String -> IO String
reactToTrafficLight light = case map toLower light of
  "red"    -> return "Stop"
  "yellow" -> return "Slow down"
  "green"  -> return "Go"
  _        -> throwIO (InvalidLight light)
  -- HC15T3: Custom Exception for Traffic Light Error

data TrafficLightException = InvalidLight String deriving (Show, Typeable)

instance Exception TrafficLightException

-- HC15T4: Exception Handler for Traffic Light

handleTrafficLight :: String -> IO ()
handleTrafficLight input = do
  result <- try (reactToTrafficLight input) :: IO (Either TrafficLightException String)
  case result of
    Right action -> putStrLn $ "Action: " ++ action
    Left (InvalidLight l) -> putStrLn $ "Error: Invalid traffic light color '" ++ l ++ "'"

-- HC15T5: Safe Division Using Maybe

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = Just (a / b)

-- HC15T6: Safe Input Parsing with readMaybe

parseDouble :: String -> Maybe Double
parseDouble = readMaybe

-- HC15T7: Velocity Calculation with Optionals and Parsing Handling

velocityMaybe :: String -> String -> Maybe Double
velocityMaybe dStr tStr = do
  d <- parseDouble dStr
  t <- parseDouble tStr
  safeDiv d t

-- HC15T8: Division with Either for Detailed Errors

safeDivEither :: Double -> Double -> Either String Double
safeDivEither _ 0 = Left "Cannot divide by zero"
safeDivEither a b = Right (a / b)

-- HC15T9: Try Function for File IO Exceptions

readFileSafe :: FilePath -> IO (Either IOError String)
readFileSafe path = try (readFile path)

-- HC15T10: Hybrid Error Handling with Either and IO

calculateVelocityHybrid :: String -> String -> IO ()
calculateVelocityHybrid dStr tStr = case (parseDouble dStr, parseDouble tStr) of
  (Nothing, _) -> putStrLn "Distance is not a valid number."
  (_, Nothing) -> putStrLn "Time is not a valid number."
  (Just d, Just t) -> case safeDivEither d t of
    Left err -> putStrLn $ "Error: " ++ err
    Right v  -> putStrLn $ "Velocity: " ++ show v ++ " units/time"

-- HC15T10: Main Function

main :: IO ()
main = do
  putStrLn "== HC15: Error Handling Demonstration =="

  -- HC15T2-4: Traffic light simulation
  putStrLn "\nTraffic light test (green):"
  handleTrafficLight "green"
  putStrLn "Traffic light test (blue):"
  handleTrafficLight "blue"

  -- HC15T5-7: Safe velocity calculation using Maybe
  putStrLn "\nVelocity using Maybe:"
  print $ velocityMaybe "100" "20"
  print $ velocityMaybe "100" "0"
  print $ velocityMaybe "abc" "10"

  -- HC15T8: Safe division with Either
  putStrLn "\nSafe division with Either:"
  print $ safeDivEither 100 20
  print $ safeDivEither 100 0

  -- HC15T9: File reading with exception handling
  putStrLn "\nReading velocity from file (velocity.txt):"
  velocityFromFile "velocity.txt"

  -- HC15T10: Hybrid input and error handling
  putStrLn "\nHybrid velocity calculation (manual input):"
  calculateVelocityHybrid "120" "0"
  calculateVelocityHybrid "foo" "30"
  calculateVelocityHybrid "120" "30"
