-- HC13T1: List Files in Directory
listFiles :: IO [FilePath]
listFiles = do
  putStrLn "Listing all files in the current directory:"
  files <- listDirectory "."
  mapM_ putStrLn files
  return files

-- HC13T2: Filter Files by Substring
filterFilesBySubstring :: String -> [FilePath] -> [FilePath]
filterFilesBySubstring substr =
  DL.filter (DL.isInfixOf substr)

-- HC13T3: Sort and Return Filtered Files
sortFilteredFiles :: String -> [FilePath] -> [FilePath]
sortFilteredFiles substr files =
  DL.sort (filterFilesBySubstring substr files)

-- HC13T6: File Names to Map
fileListToMap :: [FilePath] -> Map.Map Int FilePath
fileListToMap files = Map.fromList (zip [1..] files)

-- HC13T7: Use Custom Module in Main
runSumNonEmpty :: IO ()
runSumNonEmpty = do
  let nums = [1, 2, 3, 4]
  putStrLn $ "Sum of " ++ show nums ++ " = " ++ show (sumNonEmpty nums)

-- HC13T8: Qualified Imports for Name Conflicts
qualifiedImportDemo :: IO ()
qualifiedImportDemo = do
  let list1 = [3, 1, 2]
  putStrLn $ "Sorted list using qualified Data.List: " ++ show (DL.sort list1)
  putStrLn $ "Length using Prelude: " ++ show (length list1)
  putStrLn $ "Length using qualified Data.List: " ++ show (DL.length list1)

-- HC13T9: Renaming Module Namespace
import qualified Data.List as L
import qualified Data.Map as M

renamedModulesDemo :: IO ()
renamedModulesDemo = do
  let nums = [1, 5, 3]
  putStrLn $ "Sorted: " ++ show (L.sort nums)
  let myMap = M.fromList [(1, "A"), (2, "B")]
  putStrLn $ "Map lookup 1: " ++ show (M.lookup 1 myMap)

-- HC13T10: Multi-Module Main Function
mainFunction :: IO ()
mainFunction = do
  files <- listFiles
  putStrLn "\nEnter a substring to filter files:"
  substr <- getLine
  let filtered = sortFilteredFiles substr files
  putStrLn "\nSorted and filtered files:"
  mapM_ putStrLn filtered
  let fileMap = fileListToMap filtered
  putStrLn "\nMapped file list:"
  mapM_ print (Map.toList fileMap)

-- Main Menu
main :: IO ()
main = do
  putStrLn "\n== Haskell HC13 Task Menu =="
  putStrLn "1. List files"
  putStrLn "2. Filter files by substring"
  putStrLn "3. Sort and return filtered files"
  putStrLn "4. Use SumNonEmpty module"
  putStrLn "5. Show qualified import usage"
  putStrLn "6. Use renamed modules"
  putStrLn "7. Multi-module file search + sort"
  putStrLn "Enter choice:"
  choice <- getLine
  files <- listDirectory "."
  case choice of
    "1" -> listFiles >> return ()
    "2" -> do
      putStrLn "Enter substring:"
      substr <- getLine
      mapM_ putStrLn (filterFilesBySubstring substr files)
    "3" -> do
      putStrLn "Enter substring:"
      substr <- getLine
      mapM_ putStrLn (sortFilteredFiles substr files)
    "4" -> runSumNonEmpty
    "5" -> qualifiedImportDemo
    "6" -> renamedModulesDemo
    "7" -> mainFunction
    _   -> putStrLn "Invalid choice."
