-- HC10T1: ShowSimple Type Class
class ShowSimple a where
  showSimple :: a -> String

data PaymentMethod = CreditCard | PayPal | BankTransfer deriving (Eq, Show)

instance ShowSimple PaymentMethod where
  showSimple CreditCard   = "Credit Card"
  showSimple PayPal       = "PayPal"
  showSimple BankTransfer = "Bank Transfer"

-- HC10T2: Summable Type Class
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

-- HC10T3: Comparable Type Class
class Comparable a where
  compareWith :: a -> a -> Ordering

data Blockchain = Block Int deriving (Eq, Show)

instance Comparable Blockchain where
  compareWith (Block x) (Block y) = compare x y

-- HC10T4: Eq Instance for Box
data Box a = EmptyBox | HasBox a deriving (Show)

instance Eq a => Eq (Box a) where
  EmptyBox == EmptyBox = True
  HasBox x == HasBox y = x == y
  _ == _               = False

-- HC10T5: ShowDetailed Type Class
class ShowDetailed a where
  showDetailed :: a -> String

data User = User { userId :: Int, userName :: String } deriving (Eq)

instance ShowDetailed User where
  showDetailed (User id name) = "User(ID: " ++ show id ++ ", Name: " ++ name ++ ")"

-- HC10T6: Mutual Recursion in Eq for Blockchain
instance Eq Blockchain where
  x == y = not (x /= y)
  x /= y = case (x, y) of
    (Block a, Block b) -> a /= b

-- HC10T7: Convertible Type Class
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert = showSimple

-- HC10T8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool
  compareEquality x y = x == y

instance AdvancedEq Int
instance AdvancedEq Bool

-- HC10T9: MinMax Type Class
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = minBound
  maxValue = maxBound

-- HC10T10: Concatenatable Type Class
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable String where
  concatWith = (++)

-- Main function with tests
main :: IO ()
main = do
  putStrLn "=== HC10T1: ShowSimple PaymentMethod ==="
  print $ showSimple PayPal

  putStrLn "\n=== HC10T2: Summable Int ==="
  print $ sumUp [1, 2, 3, 4] -- Should print 10

  putStrLn "\n=== HC10T3: Comparable Blockchain ==="
  print $ compareWith (Block 3) (Block 5) -- Should print LT

  putStrLn "\n=== HC10T4: Eq for Box ==="
  print $ HasBox 5 == HasBox 5   -- True
  print $ EmptyBox == EmptyBox   -- True
  print $ HasBox 5 == EmptyBox   -- False

  putStrLn "\n=== HC10T5: ShowDetailed User ==="
  let user = User 101 "Alice"
  putStrLn $ showDetailed user

  putStrLn "\n=== HC10T6: Mutual Eq for Blockchain ==="
  print $ Block 1 == Block 1     -- True
  print $ Block 1 /= Block 2     -- True

  putStrLn "\n=== HC10T7: Convertible PaymentMethod -> String ==="
  print $ convert CreditCard :: String

  putStrLn "\n=== HC10T8: AdvancedEq ==="
  print $ compareEquality (5 :: Int) 5   -- True
  print $ compareEquality True False    -- False

  putStrLn "\n=== HC10T9: MinMax Int ==="
  print (minValue :: Int)
  print (maxValue :: Int)

  putStrLn "\n=== HC10T10: Concatenatable String ==="
  print $ concatWith "Hello, " "world!"
