import Data.List (group, sort)
import Data.Map qualified as Map
import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Test.Hspec
import Test.QuickCheck

-- Calculates the total distance between two lists
listDistance :: [Int] -> [Int] -> Int
listDistance l1 l2 = sum (zipWith (\x y -> abs (x - y)) (sort l1) (sort l2))

-- Calculates the total similarity between two lists
listSimilarity :: [Int] -> [Int] -> Int
listSimilarity l1 l2 =
  let occurences = countOccurences l2
   in sum (map (\x -> x * Map.findWithDefault 0 x occurences) l1)

-- Counts the number of occurences for every element in a list
countOccurences :: [Int] -> Map.Map Int Int
countOccurences l = Map.fromListWith (+) (map (,1) l)

-- Parses an input file as two lists
parseInput :: String -> IO ([Int], [Int])
parseInput filename = do
  content <- readFile filename
  let lineToPair line = let [a, b] = map read (words line) in (a, b)
      contentLines = lines content
      pairs = map lineToPair contentLines
  return (unzip pairs)

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      parsedLists <- parseInput inputFile
      let l1 = sort (fst parsedLists)
          l2 = sort (snd parsedLists)
          distance = listDistance l1 l2
          similarity = listSimilarity l1 l2
      print distance
      print similarity
      writeFile outputFile (unlines [show distance, show similarity])
    _ -> putStrLn "Usage: runhaskell 01.hs <inputFile> <outputFile>"

-- Unit and property tests
test :: IO ()
test = hspec $
  do
    describe "listDistance" $ do
      it "calculates the correct distance for the example input" $ do
        let l1 = [3, 4, 2, 1, 3, 3]
            l2 = [4, 3, 5, 3, 9, 3]
        listDistance l1 l2 `shouldBe` 11

      it "returns 0 for identical lists" $
        property $
          \l -> listDistance (l :: [Int]) l == 0

      it "returns 0 for empty lists" $ do
        listDistance [] [] `shouldBe` 0

      it "is always non-negative" $
        property $
          \l1 l2 -> listDistance (l1 :: [Int]) (l2 :: [Int]) >= 0

      it "is commutative" $
        property $
          \l1 l2 -> listDistance (l1 :: [Int]) (l2 :: [Int]) == listDistance l2 l1

    describe "listSimilarity" $ do
      it "calculates the correct similarity for the example input" $ do
        let l1 = [3, 4, 2, 1, 3, 3]
            l2 = [4, 3, 5, 3, 9, 3]
        listSimilarity l1 l2 `shouldBe` 31

      it "returns 0 for empty lists" $ do
        listSimilarity [] [] `shouldBe` 0
