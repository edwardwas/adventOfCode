module Day01 where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Control.Monad
import Data.Either

floorMoveParser :: Parser [Int]
floorMoveParser = (1 <$ char '(' <|> (-1) <$ char ')') `manyTill` (eof <|> void newline)

loadInstructions :: FilePath -> IO [Int]
loadInstructions = fmap (either (error . show) id . runParser floorMoveParser () "") . readFile

day1PartA :: IO Int
day1PartA = sum <$> loadInstructions "data/day1partA.txt"

day1PartB :: IO Int
day1PartB = (length . takeWhile (>= 0) . scanl (+) 0) <$> loadInstructions "data/day1partA.txt"
