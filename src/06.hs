import Data.List
import Data.Maybe (fromJust, isNothing)
import Data.Set qualified as Set
import System.Environment (getArgs, withArgs)
import System.IO (readFile, writeFile)
import Test.Hspec
import Test.QuickCheck

-- A grid of characters
type Grid = [[Char]]

-- A cardinal direction
data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

-- A coordinate position on a grid
type Position = (Int, Int, Direction)

-- Explores a grid until exiting the area or an infinite loop is detected
explore :: Grid -> Position -> Maybe (Set.Set Position)
explore grid start = keepMoving start Set.empty
  where
    keepMoving p1 positions =
      if Set.member p1 positions -- loop detection
        then Nothing
        else case move grid p1 of
          Just p2 -> keepMoving p2 (Set.insert p1 positions)
          Nothing -> Just (Set.insert p1 positions) -- out of bounds

-- Tries to move a guard one step
move :: Grid -> Position -> Maybe Position
move grid (x, y, dir)
  | newX < 0 || m <= newX || newY < 0 || n <= newY = Nothing
  | ((grid !! newY) !! newX) /= '#' = Just (newX, newY, dir)
  | otherwise = Just (x, y, newDir)
  where
    (n, m) = (length grid, length (head grid))
    (newX, newY, newDir) = case dir of
      North -> (x, y - 1, East)
      South -> (x, y + 1, West)
      East -> (x + 1, y, South)
      West -> (x - 1, y, North)

-- Finds the first position of a character in a grid
elemPos :: Char -> Grid -> Maybe (Int, Int)
elemPos elem grid = find grid 0
  where
    find [] _ = Nothing
    find (row : rows) rowIndex =
      case elemIndex elem row of
        Just colIndex -> Just (colIndex, rowIndex)
        Nothing -> find rows (rowIndex + 1)

-- Generates grid variants with one extra obstacle
generateVariants :: Grid -> [(Int, Int)] -> [Grid]
generateVariants grid positions =
  [ [ if r == row
        then [if c == col then '#' else cell | (c, cell) <- zip [0 ..] rowContent]
        else rowContent
      | (r, rowContent) <- zip [0 ..] grid
    ]
    | (col, row) <- positions
  ]

-- Parses an input file
parseInput :: String -> (Grid, Position)
parseInput content = (map (map (\c -> if c == '^' then '.' else c)) grid, start)
  where
    grid = lines content
    start = case elemPos '^' grid of
      Just (x, y) -> (x, y, North)
      Nothing -> (0, 0, North)

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run", inputFile, outputFile] -> do
      input <- readFile inputFile
      let (grid, start) = parseInput input
          (n, m) = (length grid, length (head grid))
          positions = fromJust (explore grid start)
          positionsToCoordinates s = nub (map (\(x, y, _) -> (x, y)) (Set.toList s))
          result1 = length (positionsToCoordinates positions)
          variants = generateVariants grid (positionsToCoordinates (Set.delete start positions))
          result2 = length (filter isNothing (map (`explore` start) variants))
      print result1
      print result2
      writeFile outputFile (unlines [show result1, show result2])
    ["test"] -> withArgs [] test
    _ -> putStrLn "Valid commands:\n  run <inputFile> <outputFile>\n  test"

-- Unit and property tests
test :: IO ()
test = hspec $ do
  describe "move" $ do
    it "determines moves correctly with the example input" $ do
      let grid =
            [ "....#.....",
              ".........#",
              "..........",
              "..#.......",
              ".......#..",
              "..........",
              ".#........",
              "........#.",
              "#.........",
              "......#..."
            ]
      move grid (4, 6, North) `shouldBe` Just (4, 5, North)
      move grid (2, 4, North) `shouldBe` Just (2, 4, East)
      move grid (2, 2, South) `shouldBe` Just (2, 2, West)
      move grid (1, 3, East) `shouldBe` Just (1, 3, South)
      move grid (3, 3, West) `shouldBe` Just (3, 3, North)
      move grid (0, 0, North) `shouldBe` Nothing
      move grid (9, 9, East) `shouldBe` Nothing

  describe "explore" $ do
    it "explores the correct number of positions for the example input" $ do
      let grid =
            [ "....#.....",
              ".........#",
              "..........",
              "..#.......",
              ".......#..",
              "..........",
              ".#........",
              "........#.",
              "#.........",
              "......#..."
            ]
      length (nub (map (\(x, y, _) -> (x, y)) (Set.toList (fromJust (explore grid (4, 6, North)))))) `shouldBe` 41
