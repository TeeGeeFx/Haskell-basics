{-# LANGUAGE DeriveGeneric #-}

import Data.Semigroup
import Data.Monoid
import Data.List (foldl')
import GHC.Generics (Generic)

-- HC17T1: Severity Data Type and Semigroup Instance
data Severity = Low | Medium | High | Critical deriving (Eq, Ord, Show, Enum)

instance Semigroup Severity where
  (<>) = max

-- HC17T2: Min and Max Newtypes with Semigroup
newtype Min a = Min a deriving (Show, Eq)
newtype Max a = Max a deriving (Show, Eq)

instance (Ord a) => Semigroup (Min a) where
  Min x <> Min y = Min (min x y)

instance (Ord a) => Semigroup (Max a) where
  Max x <> Max y = Max (max x y)

-- HC17T3: Monoid Instance for Severity
instance Monoid Severity where
  mempty = Low

-- HC17T4: Monoid Instance for Sum Newtype
-- Already defined in Data.Monoid as:
-- newtype Sum a = Sum { getSum :: a }
-- instance Num a => Monoid (Sum a) where mempty = Sum 0

-- HC17T5: combineLists Function
combineLists :: [Int] -> [Int] -> [Int]
combineLists = (<>)

-- HC17T6: maxSeverity Function
maxSeverity :: [Severity] -> Severity
maxSeverity = mconcat

-- HC17T7: multiplyProducts Function
-- Product is already defined in Data.Monoid
multiplyProducts :: [Product Int] -> Product Int
multiplyProducts = mconcat

-- HC17T8: foldWithSemigroup Function
foldWithSemigroup :: Semigroup a => [a] -> a
foldWithSemigroup = foldr1 (<>)

-- HC17T9: Config Data Type and Semigroup Instance
data Config = Config
  { loggingLevel :: Severity
  , timeout      :: Int    -- in seconds
  , retries      :: Int
  } deriving (Show, Eq)

instance Semigroup Config where
  Config l1 t1 r1 <> Config l2 t2 r2 =
    Config (l1 <> l2) (min t1 t2) (max r1 r2)

-- HC17T10: Monoid Instance for Config
instance Monoid Config where
  mempty = Config Low maxBound 0

-- Main function to test everything
main :: IO ()
main = do
  putStrLn "== HC17 Haskell Semigroup and Monoid Tasks =="

  putStrLn "\n[HC17T1] Combine Severity (Medium <> High):"
  print $ Medium <> High

  putStrLn "\n[HC17T2] Min and Max Semigroups:"
  print $ Min 10 <> Min 5
  print $ Max 3 <> Max 7

  putStrLn "\n[HC17T3] Monoid for Severity (mconcat [Low, Medium, High]):"
  print $ mconcat [Low, Medium, High]

  putStrLn "\n[HC17T4] Monoid for Sum:"
  print $ mconcat [Sum 1, Sum 2, Sum 3]

  putStrLn "\n[HC17T5] Combine Lists:"
  print $ combineLists [1,2,3] [4,5]

  putStrLn "\n[HC17T6] maxSeverity:"
  print $ maxSeverity [Low, Medium, High, Critical, Medium]

  putStrLn "\n[HC17T7] multiplyProducts:"
  print $ getProduct $ multiplyProducts [Product 2, Product 3, Product 4]

  putStrLn "\n[HC17T8] foldWithSemigroup:"
  print $ foldWithSemigroup [Sum 1, Sum 2, Sum 3]

  putStrLn "\n[HC17T9/10] Config Combine and Identity:"
  let config1 = Config Medium 100 3
  let config2 = Config High 60 5
  print $ config1 <> config2
  print $ config1 <> mempty
  print $ mempty <> config2
