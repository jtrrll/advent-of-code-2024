import Data.List
import Data.List.Split (splitOn)
import System.Environment (getArgs, withArgs)
import System.IO (readFile, writeFile)
import Test.Hspec
import Test.QuickCheck

-- An rule specifying the order two pages must follow
data OrderingRule = OrderingRule {before :: Int, after :: Int}
  deriving (Show, Eq)

-- Determines if an update is correct given a list of ordering rules
isCorrect :: [OrderingRule] -> [Int] -> Bool
isCorrect rules update =
  let ruleViolated (OrderingRule beforeElem afterElem) = case (elemIndex beforeElem update, elemIndex afterElem update) of
        (Just indexBefore, Just indexAfter) -> indexBefore > indexAfter
        _ -> False
   in not (any ruleViolated rules)

-- Sorts an update by a list of ordering rules
sortByRules :: [OrderingRule] -> [Int] -> [Int]
sortByRules rules update =
  let sorter x y = if OrderingRule x y `elem` rules then LT else GT
   in sortBy sorter update

-- Parses an input file
parseInput :: String -> ([OrderingRule], [[Int]])
parseInput content =
  let (rs : us : _) = splitOn [""] (lines content)
      rules = map (\line -> let [before, after] = splitOn "|" line in OrderingRule (read before) (read after)) rs
      updates = map (\line -> let xs = splitOn "," line in map read xs) us
   in (rules, updates)

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run", inputFile, outputFile] -> do
      input <- readFile inputFile
      let (rules, updates) = parseInput input
          (correct, incorrect) = partition (isCorrect rules) updates
          middleElem l = l !! div (length l) 2
          result1 = sum (map middleElem correct)
          result2 = sum (map (middleElem . sortByRules rules) incorrect)
      print result1
      print result2
      writeFile outputFile (unlines [show result1, show result2])
    ["test"] -> withArgs [] test
    _ -> putStrLn "Valid commands:\n  run <inputFile> <outputFile>\n  test"

-- Unit and property tests
test :: IO ()
test = hspec $ do
  describe "isCorrect" $ do
    it "determines which example updates are correct" $ do
      let isCorrectWithRules =
            isCorrect
              [ OrderingRule 47 53,
                OrderingRule 97 13,
                OrderingRule 97 61,
                OrderingRule 97 47,
                OrderingRule 75 29,
                OrderingRule 61 13,
                OrderingRule 75 53,
                OrderingRule 29 13,
                OrderingRule 97 29,
                OrderingRule 53 29,
                OrderingRule 61 53,
                OrderingRule 97 53,
                OrderingRule 61 29,
                OrderingRule 47 13,
                OrderingRule 75 47,
                OrderingRule 97 75,
                OrderingRule 47 61,
                OrderingRule 75 61,
                OrderingRule 47 29,
                OrderingRule 75 13,
                OrderingRule 53 13
              ]
      isCorrectWithRules [75, 47, 61, 53, 29] `shouldBe` True
      isCorrectWithRules [97, 61, 53, 29, 13] `shouldBe` True
      isCorrectWithRules [75, 29, 13] `shouldBe` True
      isCorrectWithRules [75, 97, 47, 61, 53] `shouldBe` False
      isCorrectWithRules [61, 13, 29] `shouldBe` False
      isCorrectWithRules [97, 13, 75, 29, 47] `shouldBe` False
