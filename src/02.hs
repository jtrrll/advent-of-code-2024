import Data.List (inits, tails)
import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Test.Hspec
import Test.QuickCheck

-- A list of levels
type Report = [Int]

-- Determines if a report is safe
isSafe :: Report -> Bool
isSafe [] = True
isSafe [_] = True
isSafe l =
  let diffs = zipWith (-) (tail l) l
   in (all (< 0) diffs || all (> 0) diffs)
        && all (\d -> (1 <= abs d) && (abs d <= 3)) diffs

-- Determines if a report is safe with one level removed
isSafeWithRemoval :: Report -> Bool
isSafeWithRemoval l =
  let removed = zipWith (++) (inits l) (map (drop 1) (tails l))
   in any isSafe removed

-- Parses an input file as several reports
parseInput :: String -> IO [Report]
parseInput filename = do
  content <- readFile filename
  let parseLineAsReport l = map read (words l)
  return (map parseLineAsReport (lines content))

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      reports <- parseInput inputFile
      let safeCount = length (filter isSafe reports)
          safeCountWithRemoval = length (filter isSafeWithRemoval reports)
      print safeCount
      print safeCountWithRemoval
      writeFile outputFile (unlines [show safeCount, show safeCountWithRemoval])
    _ -> putStrLn "Usage: runhaskell 02.hs <inputFile> <outputFile>"

-- Unit and property tests
test :: IO ()
test = hspec $
  do
    describe "isSafe" $ do
      it "determines safety correctly for the example input" $ do
        isSafe [7, 6, 4, 2, 1] `shouldBe` True
        isSafe [1, 2, 7, 8, 9] `shouldBe` False
        isSafe [9, 7, 6, 2, 1] `shouldBe` False
        isSafe [1, 3, 2, 4, 5] `shouldBe` False
        isSafe [8, 6, 4, 4, 1] `shouldBe` False
        isSafe [1, 3, 6, 7, 9] `shouldBe` True

      it "returns true for a report with no levels" $ do
        isSafe [] `shouldBe` True

      it "returns true for a report with one level" $
        property $
          \x -> isSafe [x :: Int] `shouldBe` True

    describe "isSafeWithRemoval" $ do
      it "determines safety correctly for the example input" $ do
        isSafeWithRemoval [7, 6, 4, 2, 1] `shouldBe` True
        isSafeWithRemoval [1, 2, 7, 8, 9] `shouldBe` False
        isSafeWithRemoval [9, 7, 6, 2, 1] `shouldBe` False
        isSafeWithRemoval [1, 3, 2, 4, 5] `shouldBe` True
        isSafeWithRemoval [8, 6, 4, 4, 1] `shouldBe` True
        isSafeWithRemoval [1, 3, 6, 7, 9] `shouldBe` True

      it "returns true for a report with no levels" $ do
        isSafeWithRemoval [] `shouldBe` True

      it "returns true for a report with one level" $
        property $
          \x -> isSafeWithRemoval [x :: Int] `shouldBe` True

      it "returns true for a report with two levels" $
        property $
          \x1 x2 -> isSafeWithRemoval [x1 :: Int, x2 :: Int] `shouldBe` True
