module Day03 where

import Util

import Linear
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Control.Monad
import Data.List

directionParser :: Parser [V2 Int]
directionParser = direction `manyTill` (void newline <|> eof)
    where direction = (V2 0 (-1) <$ char '^')
              <|> (V2 0 1 <$ char 'v')
              <|> (V2 (-1) 0 <$ char '<')
              <|> (V2 1 0 <$ char '>')

day03PartA = (length . nub . scanl (+) (V2 0 0)) <$> 
    loadParse directionParser "data/day03.txt"

takeEveryOther = map head . takeWhile (not . null) . iterate (drop 2)

day03PartB = (\x -> length $ nub $ (helper $ takeEveryOther x) ++ (helper $ takeEveryOther $ tail x)) <$>
    loadParse directionParser "data/day03.txt"
        where helper = scanl (+) (V2 0 0)
