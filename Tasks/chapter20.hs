-- HC20T1: safeDivide with Maybe Monad
safeDivide :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

-- HC20T2: sequenceMaybe for List of Maybe
sequenceMaybe :: [Maybe a] -> Maybe [a]
sequenceMaybe = foldr (\mx acc -> do x <- mx; xs <- acc; return (x:xs)) (Just [])

-- HC20T3: Writer Monad Logging Calculator
type Log = [String]

add :: Int -> Int -> Writer Log Int
add x y = do
    tell ["Adding " ++ show x ++ " and " ++ show y]
    return (x + y)

multiply :: Int -> Int -> Writer Log Int
multiply x y = do
    tell ["Multiplying " ++ show x ++ " and " ++ show y]
    return (x * y)

calculator :: Writer Log Int
calculator = do
    a <- add 2 3
    b <- multiply a 4
    return b

-- HC20T4: countChars with State Monad
countChars :: Char -> String -> Int
countChars target str = execState (mapM_ countIfTarget str) 0
  where
    countIfTarget c = when (c == target) $ modify (+1)

-- HC20T5: Reader Monad for Configurable Greeting
data Config = Config { greetingPrefix :: String }

greetUser :: String -> Reader Config String
greetUser name = do
    prefix <- asks greetingPrefix
    return (prefix ++ ", " ++ name ++ "!")
-- HC20T6: doubleMonad Combining Maybe and List
doubleMonad :: Maybe [Int] -> Maybe [Int]
doubleMonad mx = do
    xs <- mx
    return (map (*2) xs)

-- HC20T7: findFirst with Either Monad
findFirst :: (a -> Bool) -> [a] -> Either String a
findFirst _ [] = Left "Element not found"
findFirst f (x:xs)
    | f x       = Right x
    | otherwise = findFirst f xs

-- HC20T8: Parser Monad for Simple Expressions
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        return (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser pa) = Parser $ \input -> do
        (f, rest1) <- pf input
        (a, rest2) <- pa rest1
        return (f a, rest2)

instance Monad Parser where
    (Parser pa) >>= f = Parser $ \input -> do
        (a, rest1) <- pa input
        runParser (f a) rest1

charP :: Char -> Parser Char
charP c = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | x == c    = Just (x, xs)
      | otherwise = Nothing

digitP :: Parser Char
digitP = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | isDigit x = Just (x, xs)
      | otherwise = Nothing

simpleExpr :: Parser String
simpleExpr = do
    d1 <- digitP
    _ <- charP '+'
    d2 <- digitP
    return [d1, '+', d2]

-- HC20T9: replicateMonad with Identity Monad
import Data.Functor.Identity

replicateMonad :: Int -> a -> Identity [a]
replicateMonad n x = return (replicate n x)

-- HC20T10: Nested StateT and MaybeT Transformer
type Position = (Int, Int)

randomWalkStep :: Position -> MaybeT (StateT Int IO) Position
randomWalkStep (x, y) = do
    steps <- lift get
    if steps <= 0 then MaybeT (return Nothing)
    else do
        lift (put (steps - 1))
        return (x + 1, y + 1)

runWalk :: Position -> Int -> IO (Maybe Position)
runWalk start steps = evalStateT (runMaybeT (randomWalkStep start)) steps
-- HC20T11: randomWalk with State Monad
type Pos = (Int, Int)

randomWalk :: Int -> State [(Int, Int)] ()
randomWalk 0 = return ()
randomWalk n = do
  (x, y):_ <- get
  let newPos = (x + 1, y + 1)
  modify (newPos :)
  randomWalk (n - 1)

-- HC20T12: File Reading with IO Monad
readFileLines :: FilePath -> IO ()
readFileLines path = do
  contents <- readFile path
  mapM_ putStrLn (lines contents)

-- HC20T13: fibonacciMemo with State Monad
type FibCache = Map.Map Int Integer

fibonacciMemo :: Int -> State FibCache Integer
fibonacciMemo 0 = return 0
fibonacciMemo 1 = return 1
fibonacciMemo n = do
  cache <- get
  case Map.lookup n cache of
    Just result -> return result
    Nothing -> do
      a <- fibonacciMemo (n - 1)
      b <- fibonacciMemo (n - 2)
      let result = a + b
      modify (Map.insert n result)
      return result

-- HC20T14: mapMFilter Monadic Map-Filter
mapMFilter :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMFilter f = fmap catMaybes . mapM f

-- HC20T15: treeSum with Custom Monad
data Tree a = Leaf a | Node (Tree a) (Tree a)

newtype SumMonad a = SumMonad { runSum :: a }
  deriving (Functor, Applicative, Monad)

treeSum :: Num a => Tree a -> SumMonad a
treeSum (Leaf x) = return x
treeSum (Node l r) = do
  lv <- treeSum l
  rv <- treeSum r
  return (lv + rv)
 -- HC20T16: retryIO with IO Monad
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _ = return Nothing
retryIO n action = do
  result <- (Just <$> action) `catch`
  -- Main Function Demonstration
main :: IO ()
main = do
    putStrLn "HC20T1: Safe Divide 10 / 2"
    print $ safeDivide 10 2

    putStrLn "\nHC20T2: Sequence Maybe [Just 1, Just 2, Just 3]"
    print $ sequenceMaybe [Just 1, Just 2, Just 3]

    putStrLn "\nHC20T3: Logging Calculator"
    let (result, logLines) = runWriter calculator
    putStrLn $ "Result: " ++ show result
    mapM_ putStrLn logLines

    putStrLn "\nHC20T4: Count characters 'a' in \"banana\""
    print $ countChars 'a' "banana"

    putStrLn "\nHC20T5: Configurable Greeting"
    let config = Config { greetingPrefix = "Welcome" }
    putStrLn $ runReader (greetUser "Alice") config
    putStrLn "HC20T6: Double Monad - Maybe[List[Int]]"
    print $ doubleMonad (Just [1, 2, 3])

    putStrLn "\nHC20T7: Find First Element > 5"
    print $ findFirst (> 5) [1, 3, 7, 2]

    putStrLn "\nHC20T8: Simple Parser for '3+5'"
    print $ runParser simpleExpr "3+5"

    putStrLn "\nHC20T9: Replicate Monad"
    print $ runIdentity (replicateMonad 4 'A')

    putStrLn "\nHC20T10: Nested StateT and MaybeT for Random Walk"
    result <- runWalk (0, 0) 3
    putStrLn $ "Final Position: " ++ show result
    putStrLn "HC20T11: Random Walk"
  let path = execState (randomWalk 5) [(0, 0)]
  print $ reverse path

  putStrLn "\nHC20T12: File Reading (creating sample file)"
  writeFile "sample.txt" "Line 1\nLine 2\nLine 3"
  readFileLines "sample.txt"

  putStrLn "\nHC20T13: Fibonacci with Memoization"
  let (fib10, cache) = runState (fibonacciMemo 10) Map.empty
  print fib10

  putStrLn "\nHC20T14: mapMFilter Example (keep even * 10)"
  let example = mapMFilter (\x -> return $ if even x then Just (x * 10) else Nothing) [1..10]
  result <- example
  print result

  putStrLn "\nHC20T15: treeSum with Custom Monad"
  let tree = Node (Leaf 5) (Node (Leaf 3) (Leaf 2))
  print $ runSum (treeSum tree)
