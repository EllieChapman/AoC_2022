module Day5 where

import Day1
import Data.List

day5_part1 :: [String] -> String
day5_part1 as = do
    let (xs, ys) = split [] as
    let instrutions = map clean_instructions ys
    let boxes = map strip_star (transpose (map strip_line (map save_space xs)))
    take_top (steps boxes instrutions)

day5_part2 :: [String] -> String
day5_part2 as = do
    let (xs, ys) = split [] as
    let instrutions = map clean_instructions ys
    let boxes = map strip_star (transpose (map strip_line (map save_space xs)))
    take_top (steps2 boxes instrutions)

take_top :: [String] -> String
take_top xs =
    case xs of
        x:xs -> head x : (take_top xs)
        [] -> []

steps :: [String] -> [(Int, Int, Int)] -> [String]
steps xs instructions = do
    case instructions of
        i:is ->
            case i of
                (a,b,c) ->
                    if a > 0
                    then
                        steps (step xs (b, c)) ((a-1, b, c):is)
                    else
                        steps xs is
        [] -> xs


steps2 :: [String] -> [(Int, Int, Int)] -> [String]
steps2 xs instructions = do
    case instructions of
        i:is ->
            steps2 (step2 xs i) is
            -- case i of
            --     (a,b,c) ->
            --         if a > 0
            --         then
            --             steps2 (step2 xs (b, c)) ((a-1, b, c):is)
            --         else
            --             steps2 xs is
        [] -> xs



pop :: [String] -> [String] -> Int -> (Char, [String])
pop store xs n =
    case xs of
        x:xs ->
            if n == 1
            then
                case x of
                    y:ys -> (y, store ++ (ys:xs))
                    [] -> error "trying to pop from empty string"
            else pop (store ++ [x]) xs (n-1)
        [] -> error "shound't get here"

push :: [String] -> [String] -> Int -> Char-> [String]
push store xs n c =
    case xs of
        x:xs ->
            if n == 1
            then store ++ ((c:x):xs)
                -- case x of
                --     y:ys -> (y, store ++ (ys:xs))
                --     [] -> error "trying to pop from empty string"
            else push (store ++ [x]) xs (n-1) c
        [] -> error "shound't get here in push"

pop2 :: [String] -> [String] -> Int -> Int -> (String, [String])
pop2 store xs n l =
    case xs of
        x:xs ->
            if n == 1
            then
                (take l x, store ++ ((drop l x):xs))
            else pop2 (store ++ [x]) xs (n-1) l
        [] -> error "shound't get here"

push2 :: [String] -> [String] -> Int -> String -> [String]
push2 store xs n s =
    case xs of
        x:xs ->
            if n == 1
            then store ++ ((s ++ x):xs)
            else push2 (store ++ [x]) xs (n-1) s
        [] -> error "shound't get here in push"

step :: [String] -> (Int, Int) -> [String]
step xs (src, dst) = do
    let (c, ys) = pop [] xs src
    push [] ys dst c


step2 :: [String] -> (Int, Int, Int) -> [String]
step2 xs (l, src, dst) = do
    let (s, ys) = pop2 [] xs src l
    push2 [] ys dst s


save_space :: String -> String
save_space xs =
    case xs of
        x:y:z:z2:xs ->
            if (x==' ') && (y==' ') && (z==' ') && (z2==' ')
            then '*':save_space xs
            else x : save_space (y:(z:(z2:xs)))
        x:xs -> x : save_space xs
        [] -> []

strip_line :: String -> String
strip_line xs =
    case xs of
        x:xs ->
            if (x /= ' ') && (x /= '[') && (x /= ']')
            then x : strip_line xs
            else strip_line xs
        [] -> []

strip_star :: String -> String
strip_star xs =
    case xs of
        x:xs ->
            if (x == '*')
            then strip_star xs
            else x : strip_star xs
        [] -> []


clean_instructions :: String -> (Int, Int, Int)
clean_instructions xs = do
    let _a:b:_c:d:_e:f:_ys = splitOn ' ' xs
    (read b, read d, read f)


split :: [String] -> [String] -> ([String], [String])
split store original =
    case original of
        x:xs ->
            case x of
                _y:_ys ->
                    split (store ++ [x]) xs
                _ ->
                    (init store, xs)
        [] -> error "reached end of input, failed to parse"