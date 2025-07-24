-- HC19T1: Applicative Instance for Pair
data Pair a = Pair a a deriving (Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    Pair f g <*> Pair x y = Pair (f x) (g y)

-- HC19T2: addThreeApplicative Function
addThreeApplicative :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
addThreeApplicative = liftA3 (\x y z -> x + y + z)

-- HC19T3: safeProduct for Maybe Int
safeProduct :: [Maybe Int] -> Maybe Int
safeProduct = foldr (liftA2 (*)) (Just 1)

-- HC19T4: liftAndMultiply with liftA2
liftAndMultiply :: Maybe Int -> Maybe Int -> Maybe Int
liftAndMultiply = liftA2 (*)

-- HC19T5: applyEffects with <*>
applyEffects :: (IO Int, IO Int) -> IO Int
applyEffects (a, b) = (+) <$> a <*> b

-- HC19T6: repeatEffect with forever
repeatEffect :: IO () -> IO ()
repeatEffect = forever

-- HC19T7: conditionalPrint with when
conditionalPrint :: Bool -> IO ()
conditionalPrint condition = when condition $ putStrLn "Condition is True!"

-- HC19T8: discardSecond with <*
discardSecond :: IO a -> IO b -> IO a
discardSecond = (<*)

-- HC19T9: pureAndApply Demonstration
pureAndApply :: IO ()
pureAndApply = do
    let result = pure (+) <*> Just 3 <*> Just 4
    print result

-- HC19T10: combineResults for Either
combineResults :: Either String Int -> Either String Int -> Either String Int
combineResults = liftA2 (+)

-- Demo main
main :: IO ()
main = do
    putStrLn "\n[HC19T1] Applicative for Pair:"
    print $ Pair (+1) (*2) <*> Pair 3 4

    putStrLn "\n[HC19T2] addThreeApplicative:"
    print $ addThreeApplicative (Just 1) (Just 2) (Just 3)
    print $ addThreeApplicative (Just 1) Nothing (Just 3)

    putStrLn "\n[HC19T3] safeProduct:"
    print $ safeProduct [Just 2, Just 3, Just 4]
    print $ safeProduct [Just 2, Nothing, Just 4]

    putStrLn "\n[HC19T4] liftAndMultiply:"
    print $ liftAndMultiply (Just 5) (Just 6)

    putStrLn "\n[HC19T5] applyEffects:"
    sumResult <- applyEffects (readIO "10", readIO "20")
    print $ "Sum of effects: " ++ show sumResult

    putStrLn "\n[HC19T6] repeatEffect (printing once only):"
    -- We'll simulate a single call instead of forever loop:
    repeatEffect (putStrLn "Repeating once for demo") >> return ()

    putStrLn "\n[HC19T7] conditionalPrint:"
    conditionalPrint True
    conditionalPrint False

    putStrLn "\n[HC19T8] discardSecond:"
    result <- discardSecond (putStrLn "First") (putStrLn "Second")
    print result

    putStrLn "\n[HC19T9] pureAndApply:"
    pureAndApply

    putStrLn "\n[HC19T10] combineResults:"
    print $ combineResults (Right 5) (Right 10)
    print $ combineResults (Left "Error 1") (Right 10)
