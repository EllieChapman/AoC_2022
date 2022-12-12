module Day11p2 where

import qualified Day1
import Data.List


day11_part2 :: [String] -> IO Int
day11_part2 xs = do
    -- print (day11_part1 xs)
    res <- (game (parse_monkeys xs))
    let [a,b] = (take 2 (reverse (sort res)))
    pure (a*b)


-- items, (op, rhs) divisor m_if_true m_if_false           (rhs is -1 if old again)
data Monkey = Mky [Int] (Char, Int) Int Int Int deriving (Show)

parse_monkeys :: [String] -> [Monkey]
parse_monkeys xs =
    if length xs > 0
    then (parse_monkey (take 6 xs)):(parse_monkeys (drop 7 xs))
    else []


parse_monkey :: [String] -> Monkey
parse_monkey xs = do
    let xxs = map (Day1.splitOn ' ') xs
    let _a:b:c:d:e:f:_xs = xxs
    let is = map read (map remove_commas (drop 4 b))
    let ops = ((head (head (drop 6 c))), (parse_rhs (head (drop 7 c))))
    let divisor = read (head (reverse d))
    let if_t = read (head (reverse e))
    let if_f = read (head (reverse f))
    Mky is ops divisor if_t if_f


remove_commas :: String -> String
remove_commas xs =
    case xs of
        x:xs ->
            if x == ','
            then remove_commas xs
            else x:(remove_commas xs)
        [] -> []

parse_rhs :: String -> Int
parse_rhs s =
    if s == "old"
    then -1
    else read s


game :: [Monkey] -> IO [Int]
game ms = do
    res :: [Int] <- one_round ms 1 (take (length ms) (repeat 0))
    -- print ("final res ", res)
    pure res

-- calla one moneky then recurses until recieves n which is 20 h=then returnes
one_round :: [Monkey] -> Int -> [Int] -> IO [Int]
one_round ms round_n moves = do
    -- print ms
    if round_n > 10000
    then pure moves
    else do
        (new_ms, new_moves) <- one_monkey ms 1 moves
        one_round new_ms (round_n + 1) new_moves

size :: Monkey -> Int
size m =
    case m of
        Mky a _ _ _ _ ->
            length a


-- ebc nb monkey id needs to start at 1
one_monkey :: [Monkey] -> Int -> [Int] -> IO ([Monkey], [Int])
one_monkey orig_ms monkey_id totals = do
    -- print ("original ", orig_ms)
    if monkey_id <= (length orig_ms)
    then do
        let m = head (drop (monkey_id -1) orig_ms)
        if monkey_done m
        then do
            -- print "monkey done"
            one_monkey orig_ms (monkey_id + 1) totals
        else do
            let final_ms = catch_one (throw_one orig_ms monkey_id)
            let new_totals = (take (monkey_id -1) totals) ++ [1 + (get_first (drop (monkey_id-1) totals))] ++ (drop monkey_id totals)
            one_monkey final_ms monkey_id new_totals
    else
        pure (orig_ms, totals)


get_first_remaining :: [Monkey] -> [Monkey]
get_first_remaining ms =
    case ms of
        m:_ms -> [m]
        [] -> []

get_first :: [Int] -> Int
get_first is =
    case is of
        i:_is -> i
        [] -> 0

monkey_done :: Monkey -> Bool
monkey_done m =
    case m of
        Mky as _ _ _ _ ->
            case as of
                _a:_as -> False 
                [] -> True


throw_one :: [Monkey] -> Int -> ([Monkey], (Int, Int))
throw_one ms monkey_id = do
    let m = head (drop (monkey_id -1) ms)
    case m of
        Mky is ops d if_t if_f ->
            case is of
                object:os -> do
                    let (op, rhs) = ops
                    let real_rhs = get_rhs object rhs
                    let ans = (get_ans object op real_rhs) `mod` 9699690 -- nb this is the hardcoded lcm of the 7 divisors
                    let thrown_m = Mky os ops d if_t if_f
                    let new_ms = (take (monkey_id -1) ms) ++ [thrown_m] ++ (drop monkey_id ms)
                    if ans `mod` d == 0
                    then (new_ms, (ans, if_t))
                    else (new_ms, (ans, if_f))
                [] -> error "should only call throw when have item to throw"


-- catch one is returned id as in text file, so id needs 1 added to it
catch_one :: ([Monkey], (Int, Int)) -> [Monkey]
catch_one (ms, (i, monkey_id)) = do
    let m = head (drop (monkey_id) ms)
    case m of
        Mky is ops d if_t if_f -> do
            let caught_m = Mky (is++[i]) ops d if_t if_f
            (take (monkey_id) ms) ++ [caught_m] ++ (drop (monkey_id+1) ms)



get_rhs :: Int -> Int -> Int
get_rhs i rhs =
    if rhs > 0
    then rhs
    else i


get_ans :: Int -> Char -> Int -> Int
get_ans i op rhs =
    if op == '+'
    then i + rhs
    else i * rhs
