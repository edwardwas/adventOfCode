module Day06 where

import Util

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Control.Monad
import qualified Data.Map.Strict as M

data Action = Action {
      changeFunc :: Bool -> Bool
    , lowerCorner :: (Int,Int)
    , upperCorner :: (Int,Int)
    } 

parseCoord :: Parser (Int,Int)
parseCoord = (\x y -> (read x, read y)) <$>
    manyTill digit (char ',') <*> manyTill digit (void newline <|> void space <|> eof)

parseActionFunc :: Parser (Bool -> Bool)
parseActionFunc =
        try (const True <$ string "turn on")
    <|> try (const False <$ string "turn off")
    <|> (not <$ string "toggle")

parseAction :: Parser Action
parseAction = do
    actionFunc <- parseActionFunc
    spaces
    lCorner <- parseCoord
    string "through"
    spaces
    tCorner <- parseCoord
    return $ Action actionFunc lCorner tCorner

applyAction :: Action -> M.Map (Int,Int) Bool -> M.Map (Int,Int) Bool
applyAction (Action func (x1,y1) (x2,y2)) m =
    foldr (M.adjust func) m $ (,) <$> [x1 .. x2] <*> [y1 .. y2]

startMap = M.fromList [((x,y),False) | x <- [0..999], y <- [0..999]]

day06PartA :: IO Int
day06PartA = (length . M.filter id . foldr applyAction startMap) <$> 
    loadParse (manyTill parseAction eof) "data/day06.txt"
