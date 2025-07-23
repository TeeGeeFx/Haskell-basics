-- HC9T1: Define a Parametric Type Synonym
type Entity a = (String, a)  -- Example: ("Name", Address)

-- HC9T2: Implement a Parametric Data Type
data Box a = Empty | Has a deriving (Show)

-- HC9T3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN n (Has x) = Has (n + x)
addN _ Empty   = Empty

-- HC9T4: Extract a Value from a Box
extract :: a -> Box a -> a
extract _ (Has x) = x
extract def Empty = def

-- HC9T5: Parametric Data Type with Record Syntax
data Shape a = Circle { color :: a }
             | Rectangle { color :: a }
             deriving (Show)

-- HC9T6: Recursive Data Type for Tweets
data Tweet = Tweet
  { content  :: String
  , likes    :: Int
  , comments :: [Tweet]
  } deriving (Show)

-- HC9T7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement (Tweet _ l cs) = l + sum (map engagement cs)

-- HC9T8: Recursive Sequence Data Type
data Sequence a = End | Node a (Sequence a) deriving (Show)

-- HC9T9: Check for Element in a Sequence
elemSeq :: Eq a => a -> Sequence a -> Bool
elemSeq _ End = False
elemSeq e (Node x xs)
  | e == x    = True
  | otherwise = elemSeq e xs

-- HC9T10: Binary Search Tree Data Type
data BST a = EmptyTree
           | NodeBST a (BST a) (BST a)
           deriving (Show)

-- Optional utility to insert into BST
insertBST :: Ord a => a -> BST a -> BST a
insertBST x EmptyTree = NodeBST x EmptyTree EmptyTree
insertBST x (NodeBST y left right)
  | x < y     = NodeBST y (insertBST x left) right
  | x > y     = NodeBST y left (insertBST x right)
  | otherwise = NodeBST y left right

-- Main function to demonstrate examples
main :: IO ()
main = do
  putStrLn "=== HC9T3: addN Example ==="
  print $ addN 5 (Has 10)   -- Should print Has 15
  print $ addN 5 Empty      -- Should print Empty

  putStrLn "\n=== HC9T4: extract Example ==="
  print $ extract 0 (Has 7) -- Should print 7
  print $ extract 0 Empty   -- Should print 0

  putStrLn "\n=== HC9T5: Shape Examples ==="
  print $ Circle { color = "Red" }
  print $ Rectangle { color = "Blue" }

  putStrLn "\n=== HC9T7: Tweet Engagement Example ==="
  let comment1 = Tweet "Nice!" 2 []
  let comment2 = Tweet "Cool!" 3 []
  let mainTweet = Tweet "Hello World" 10 [comment1, comment2]
  print $ engagement mainTweet  -- Should print 15

  putStrLn "\n=== HC9T9: Sequence Element Check ==="
  let seq1 = Node 1 (Node 2 (Node 3 End))
  print $ elemSeq 2 seq1  -- Should print True
  print $ elemSeq 4 seq1  -- Should print False

  putStrLn "\n=== HC9T10: BST Insert and Display ==="
  let tree = foldr insertBST EmptyTree [5, 3, 7, 2, 4]
  print tree
