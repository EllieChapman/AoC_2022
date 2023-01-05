module Day9 (main) where

import Utils (readLinesFromFile, check)
import qualified Data.Set as Set (fromList, size)

main :: IO ()
main = do
    putStrLn "*Day 9 Part 1*"
    _bs <- readLinesFromFile "src/day9_input.txt"
    res <- day9_part1 _bs
    print (check 13 (res))

day9_part1 :: [String] -> IO (Int)
day9_part1 xs = do
    let ys = map parse xs
    -- let _dirs :: [Direction] = [d | (i, d1) <- ys, d <- replicate i d1]
    let dirs :: [Direction] = concat (map (\(d, i) -> replicate i d) ys)
    let state0 = StateC {head = (0,0), tail = (0,0)}
    let all_states = steps state0 dirs
    mapM_ print all_states
    pure (Set.size (Set.fromList (map (\StateC {tail} -> tail) all_states)))

data Direction = U | D | R | L

make_direction :: Char -> Direction
make_direction = \case
    'L' -> L
    'R' -> R
    'D' -> D
    'U' -> U
    _ -> error "unexpected case"

parse :: String -> (Direction, Int)
parse xs =
    (make_direction (Prelude.head xs), read (drop 2 xs))

type Coord = (Int, Int)

data State = StateC { head :: Coord, tail :: Coord} deriving (Eq, Ord, Show)

steps :: State -> [Direction] -> [State]
steps s ds =
    case ds of
        x:xs -> s:(steps (step s x) xs)
        [] -> [s]

step :: State -> Direction -> State
step StateC{head=h0, tail=t0} d = do
    let head = move_head h0 d
    let tail = move_tail head t0
    StateC {head, tail}


move_head :: Coord -> Direction -> Coord
move_head (x, y) = \case
    U -> (x, y+1)
    D -> (x, y-1)
    L -> (x-1, y)
    R -> (x+1, y)

move_tail :: Coord -> Coord -> Coord
move_tail (hx, hy) (tx, ty) = do
    let xdiff = hx - tx
    let ydiff = hy - ty
    let xdir = dir_to_move hx tx
    let ydir = dir_to_move hy ty
    if xdiff >= 2 || xdiff <= -2
    then (tx + xdir, ty + ydir)
        -- if ydiff == 0
        -- then --
        -- else --
    else
        if ydiff >= 2 || ydiff <= -2
        then (tx + xdir, ty + ydir)
            -- if xdiff == 0
            -- then --
            -- else --
        else (tx, ty)

dir_to_move :: Int -> Int -> Int
dir_to_move h t = if
    | h > t -> 1
    | h < t -> -1
    | h == t -> 0
    | otherwise -> error "no otherwose case"

    -- if x    >= 2
    --     y same
            -- step x
    --     y diff
            -- step x and y

    -- elif y > = 2
    --     x same
            -- step y
    --     x diff
            -- step x and y
