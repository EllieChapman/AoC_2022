module Day1 where

import Data.List

day1_part1 :: [String] -> Int
day1_part1 = maximum . map sum . map (map intify) . splitOn ""

addone :: Int -> Int
addone = \x -> x+1

square :: Int -> Int
square = \x -> x*x

addonethensquare :: Int -> Int
addonethensquare = square . addone . square

compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f(g x)


parse :: [String] -> [[String]]
parse xs = do
    let (as, bs) = chop_one xs
    if bs == []
    then [as]
    else as:parse bs


chop_one :: [String] -> ([String], [String])
chop_one xs =
    case xs of 
        x:xs ->
            if x == ""
            then ([], xs)
            else let (as, bs) = chop_one xs in
                (x:as, bs)
                
        [] -> ([], [])

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
    where go [] acc = [reverse acc]
          go (y : ys) acc = if x == y
                            then reverse acc : go ys []
                            else go ys (y : acc)

intify :: String -> Int
intify = read

takeopposite :: [a] -> Int -> [a]
takeopposite as i = take i as

myflip :: (a->b->c) -> b -> a -> c
myflip f b a = f a b


day1_part2 :: [String] -> Int
day1_part2 = sum . myflip takeopposite 3 . reverse . sort . map sum . map (map intify) . parse