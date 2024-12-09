import Data.List
import Data.Universe.Helpers (diagonals)
import System.Environment (getArgs, withArgs)
import System.IO (readFile, writeFile)
import Test.Hspec
import Test.QuickCheck

-- A grid of characters
type Grid = [[Char]]

-- Determines the number of occurences of a word in a grid
gridSearch :: String -> Grid -> Int
gridSearch word grid =
  let rows = grid
      cols = transpose grid
      diags = diagonals grid ++ diagonals (map reverse grid)
      all = rows ++ map reverse rows ++ cols ++ map reverse cols ++ diags ++ map reverse diags
   in sum (map (wordSearch word) all)

-- Searches for occurences of a word in a string
wordSearch :: String -> String -> Int
wordSearch word string = length (filter (isPrefixOf word) (tails string))

-- Determines the number of occurences of an X pattern in a grid
gridXSearch :: String -> Grid -> Int
gridXSearch word grid
  | even (length word) = 0
  | otherwise =
      let xs = [word ++ word, word ++ reverse word, reverse word ++ word, reverse word ++ reverse word]
       in length (filter (`elem` xs) (extractAllX grid))

-- Extracts all X patterns from a grid
extractAllX :: Grid -> [[Char]]
extractAllX grid = extractX 0 0
  where
    (n, m) = (length grid, length (head grid))
    extractX i j
      | i > n - 3 = []
      | j > m - 3 = extractX (i + 1) 0
      | otherwise =
          map
            (\(i, j) -> (grid !! i) !! j)
            [ (i, j),
              (i + 1, j + 1),
              (i + 2, j + 2),
              (i + 2, j),
              (i + 1, j + 1),
              (i, j + 2)
            ]
            : extractX i (j + 1)

-- Parses an input file
parseInput :: String -> IO Grid
parseInput filename = do
  content <- readFile filename
  return (lines content)

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run", inputFile, outputFile] -> do
      grid <- parseInput inputFile
      let result1 = gridSearch "XMAS" grid
          result2 = gridXSearch "MAS" grid
      print result1
      print result2
      writeFile outputFile (unlines [show result1, show result2])
    ["test"] -> withArgs [] test
    _ -> putStrLn "Valid commands:\n  run <inputFile> <outputFile>\n  test"

-- Unit and property tests
test :: IO ()
test = hspec $ do
  describe "gridSearch" $ do
    it "finds the correct number of words for the example input" $ do
      gridSearch
        "XMAS"
        [ "..X...",
          ".SAMX.",
          ".A..A.",
          "XMAS.S",
          ".X...."
        ]
        `shouldBe` 4
      gridSearch
        "XMAS"
        [ "MMMSXXMASM",
          "MSAMXMSMSA",
          "AMXSXMAAMM",
          "MSAMASMSMX",
          "XMASAMXAMM",
          "XXAMMXXAMA",
          "SMSMSASXSS",
          "SAXAMASAAA",
          "MAMMMXMMMM",
          "MXMXAXMASX"
        ]
        `shouldBe` 18
      gridSearch
        "XMAS"
        [ "....XXMAS.",
          ".SAMXMS...",
          "...S..A...",
          "..A.A.MS.X",
          "XMASAMX.MM",
          "X.....XA.A",
          "S.S.S.S.SS",
          ".A.A.A.A.A",
          "..M.M.M.MM",
          ".X.X.XMASX"
        ]
        `shouldBe` 18

    it "returns zero for a grid with no matches" $ do
      gridSearch "word" [""] `shouldBe` 0

  describe "gridXSearch" $ do
    it "finds the correct number of words for the example input" $ do
      gridXSearch
        "MAS"
        [ "M.S",
          ".A.",
          "M.S"
        ]
        `shouldBe` 1
      gridXSearch
        "MAS"
        [ ".M.S......",
          "..A..MSMS.",
          ".M.S.MAA..",
          "..A.ASMSM.",
          ".M.S.M....",
          "..........",
          "S.S.S.S.S.",
          ".A.A.A.A..",
          "M.M.M.M.M.",
          ".........."
        ]
        `shouldBe` 9

    it "returns zero for a grid with no matches" $ do
      gridXSearch "word" [""] `shouldBe` 0

  describe "wordSearch" $ do
    it "finds the correct number of words for the example input" $ do
      wordSearch "XMAS" "XMAS." `shouldBe` 1
      wordSearch "XMAS" "XMAS.S" `shouldBe` 1
      wordSearch "XMAS" "XMAS" `shouldBe` 1
      wordSearch "XMAS" "MMMSXXMASM" `shouldBe` 1

    it "returns zero for a word with no matches" $ do
      wordSearch "word" "" `shouldBe` 0
