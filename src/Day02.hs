module Day02 where

import Util

import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

instructionParser :: Parser [(Int,Int,Int)]
instructionParser = many $ (\x y z -> (read x, read y, read z)) 
    <$> digit `manyTill` char 'x' <*> digit `manyTill` char 'x' <*> digit `manyTill` (eof <|> void newline)

loadInstructions :: FilePath -> IO [(Int,Int,Int)]
loadInstructions = fmap (either (error . show) id . runParser instructionParser () "") . readFile

paperNeeded :: (Int,Int,Int) -> Int
paperNeeded (l,w,h) = 2 * (sum xs) + minimum xs
    where xs = [l*w, h*w, l*h]

day02PartA :: IO Int
day02PartA = (sum . map paperNeeded) <$> loadInstructions "data/day2.txt"

ribbonNeeded :: (Int,Int,Int) -> Int
ribbonNeeded (l,w,h) = (l*w*h) + 2 * (sum $ init $ sort [l,w,h])

day02PartB :: IO Int
day02PartB = (sum . map ribbonNeeded) <$> loadInstructions "data/day2.txt"
