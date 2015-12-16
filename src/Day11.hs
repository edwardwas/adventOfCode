module Day11 where

input = "hxbxwxba"

increment :: String -> String
increment = helper
    where helper (x:xs)
            | x == 'z' = 'a' : helper xs
            | otherwise = succ x : xs
          helper [] = []

listContains :: Eq a => [a] -> [a] -> Bool
listContains test = any helper . takeWhile ([] /=) . iterate tail
    where helper ys = test == take (length test ) ys

straightTest :: String -> Bool
straightTest xs = any (flip listContains xs) posList
    where posList = map (\x -> [x, pred x, pred (pred x)]) ['c' .. 'z']

confusingLetters :: String -> Bool
confusingLetters xs = not $ any (\c -> any (== c) xs) ['i','o','l']

pairCount :: Eq a => [a] -> Bool
pairCount xs = helper xs >= 2
    where helper (x:y:xs)
            | x == y = 1 + helper xs
            | otherwise = helper xs
          helper _ = 0

testString :: String -> Bool
testString xs = straightTest xs && confusingLetters xs && pairCount xs

partA = reverse . head . filter testString . iterate increment . reverse 

day11PartA :: IO String
day11PartA = return $ partA input
