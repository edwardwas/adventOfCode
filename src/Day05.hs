module Day05 where

import qualified Data.Map as M

vowelTest :: String -> Bool
vowelTest xs = 3 <= length (filter id $ (==) <$> xs <*> "aeiou")

twoTheSame :: String -> Bool
twoTheSame xs = or $ zipWith (==) xs $ tail xs

badStrings :: String -> Bool
badStrings xs = not $ or $ map (\s -> helper s xs) ["ab","cd","pq","xy"]
    where helper a = any (== a) . map (take (length a)) . takeWhile ([] /=)
                . iterate tail

stringIsNice :: String -> Bool
stringIsNice a = vowelTest a && twoTheSame a && badStrings a

day05PartA :: IO Int
day05PartA = (length . filter id . map stringIsNice . lines) <$> readFile "data/day05.txt"

twicePair :: String -> Bool
twicePair = any (>= 2) . M.elems . foldr (\k -> M.insertWith (+) k 1) M.empty .
    map (take 2) . init . takeWhile ([] /=) . iterate tail 

repeatBetween :: String -> Bool
repeatBetween = any helper . takeWhile (\x -> length x >= 3) . iterate (tail)
    where helper (a:b:c:_) = a == b && c /= b

day05PartB :: IO Int
day05PartB = (length . filter id . map (\x -> repeatBetween x && twicePair x) . lines)
    <$> readFile "data/day05.txt"
    
