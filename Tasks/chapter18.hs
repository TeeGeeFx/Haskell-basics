-- HC18T1: mapToLower Function with fmap
mapToLower :: String -> String
mapToLower = fmap toLower

-- HC18T2: Functor Instance for Tree
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

-- HC18T3: incrementTreeValues Function
incrementTreeValues :: Tree Int -> Tree Int
incrementTreeValues = fmap (+1)

-- HC18T4: mapToBits Function
mapToBits :: [Bool] -> [Char]
mapToBits = fmap (\b -> if b then '1' else '0')

-- HC18T5: Functor Instance for Either
-- Already in Prelude, but for educational purposes:
-- instance Functor (Either e) where
--     fmap f (Right x) = Right (f x)
--     fmap _ (Left e)  = Left e

-- HC18T6: applyToMaybe Function
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe = fmap

-- HC18T7: fmapTuple Function
fmapTuple :: (b -> c) -> (a, b) -> (a, c)
fmapTuple = fmap

-- HC18T8: identityLawCheck Function
identityLawCheck :: (Eq (f a), Functor f) => f a -> Bool
identityLawCheck x = fmap id x == x

-- HC18T9: compositionLawCheck Function
compositionLawCheck :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
compositionLawCheck f g x = fmap (f . g) x == (fmap f . fmap g) x

-- HC18T10: nestedFmap Function
nestedFmap :: (a -> b) -> [[Maybe a]] -> [[Maybe b]]
nestedFmap = fmap . fmap . fmap

-- Demo Main Function
main :: IO ()
main = do
  putStrLn "== HC18 Functor Tasks =="

  -- HC18T1
  putStrLn "\n[HC18T1] mapToLower:"
  print $ mapToLower "Hello, WORLD"

  -- HC18T2 / T3
  let tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
  putStrLn "\n[HC18T2/T3] incrementTreeValues on Tree:"
  print $ incrementTreeValues tree

  -- HC18T4
  putStrLn "\n[HC18T4] mapToBits:"
  print $ mapToBits [True, False, True, True]

  -- HC18T5
  putStrLn "\n[HC18T5] Functor for Either:"
  print $ fmap (+1) (Right 10 :: Either String Int)
  print $ fmap (+1) (Left "Error" :: Either String Int)

  -- HC18T6
  putStrLn "\n[HC18T6] applyToMaybe:"
  print $ applyToMaybe (*2) (Just 5)
  print $ applyToMaybe (*2) Nothing

  -- HC18T7
  putStrLn "\n[HC18T7] fmapTuple:"
  print $ fmapTuple length ("word", "hello")

  -- HC18T8
  putStrLn "\n[HC18T8] identityLawCheck:"
  print $ identityLawCheck (Just 42)
  print $ identityLawCheck [1,2,3]
  print $ identityLawCheck (Right "hello" :: Either String String)

  -- HC18T9
  putStrLn "\n[HC18T9] compositionLawCheck:"
  print $ compositionLawCheck (+1) (*2) (Just 3)
  print $ compositionLawCheck (++"!") reverse (Just "wow")

  -- HC18T10
  putStrLn "\n[HC18T10] nestedFmap:"
  let nested = [[Just 1, Nothing], [Just 2]]
  print $ nestedFmap (+1) nested
