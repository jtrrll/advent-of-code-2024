{-# LANGUAGE LambdaCase #-}

import Data.List
import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Test.Hspec
import Test.QuickCheck
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

-- An instruction
data Instruction = Mul {x :: Int, y :: Int} | Do | Don't
  deriving (Show, Eq)

-- Extracts instructions from garbled memory
extract :: String -> [Instruction]
extract memory =
  let matches = getAllTextMatches (memory =~ "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)" :: AllTextMatches [] String)
      fromString s
        | isPrefixOf "mul" s =
            let (x, ',' : y) = break (== ',') (drop 4 (init s))
             in Mul (read x) (read y)
        | isPrefixOf "don't" s = Don't
        | isPrefixOf "do" s = Do
        | otherwise = error "unknown instruction"
   in map fromString matches

-- Executes a list of instructions
execute :: [Instruction] -> Int
execute [] = 0
execute (Mul x y : rest) = x * y + execute rest
execute (Do : rest) = execute rest
execute (Don't : rest) = execute (dropUntilDo rest)
  where
    dropUntilDo :: [Instruction] -> [Instruction]
    dropUntilDo [] = []
    dropUntilDo (Do : rest) = rest
    dropUntilDo (_ : rest) = dropUntilDo rest

-- Parses an input file
parseInput :: String -> IO String
parseInput filename = do readFile filename

-- Main function
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      memory <- parseInput inputFile
      let instructions = extract memory
          mulInstructions =
            filter
              ( \case
                  Mul _ _ -> True
                  _ -> False
              )
              instructions
          result1 = execute mulInstructions
          result2 = execute instructions
      print result1
      print result2
      writeFile outputFile (unlines [show result1, show result2])
    _ -> putStrLn "Usage: runhaskell 03.hs <inputFile> <outputFile>"

-- Unit and property tests
test :: IO ()
test = hspec $
  do
    describe "extract" $ do
      it "extracts valid instructions for the example input" $ do
        extract "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))" `shouldBe` [Mul 2 4, Mul 5 5, Mul 11 8, Mul 8 5]
        extract "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))" `shouldBe` [Mul 2 4, Don't, Mul 5 5, Mul 11 8, Do, Mul 8 5]

      it "returns nothing for a string with zero instructions" $ do
        extract "" `shouldBe` []

    describe "execute" $ do
      it "calculates the correct result for the example input" $ do
        execute [Mul 2 4, Mul 5 5, Mul 11 8, Mul 8 5] `shouldBe` 161
        execute [Mul 2 4, Don't, Mul 5 5, Mul 11 8, Do, Mul 8 5] `shouldBe` 48

      it "returns zero for a list of no instructions" $ do
        execute [] `shouldBe` 0
