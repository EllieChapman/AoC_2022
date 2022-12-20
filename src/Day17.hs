module Day17 where

import Data.Set (Set, map, fromList, union, intersection)


day17_part1 :: [String] -> Int
day17_part1 xs = do
    let modified_line = add_downs (head xs)
    let instructions = ("", modified_line) -- done and todo instruction strings
    simulate_n_rocks instructions 1 2022 (fromList [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0)])


-- eventually will return max height after n rocks simulated. Start will passing in 1 and rocks to simukate
simulate_n_rocks :: (String, String) -> Int -> Int -> Set (Int, Int) -> Int
simulate_n_rocks instructions rock_number rocks_to_simulate columns =
    if rock_number <= rocks_to_simulate
    then do
        let rock = get_next_shape rock_number (get_max_height columns)
        let (new_instructions, new_columns) = do_one_rock instructions rock columns
        simulate_n_rocks new_instructions (rock_number + 1) rocks_to_simulate new_columns
    else get_max_height columns


-- returns updated instructions tuple and updated columns once rock is landed
do_one_rock :: (String, String) -> Set (Int, Int) -> Set (Int, Int) -> ((String, String), Set (Int, Int))
do_one_rock instructions points columns = do
    let (new_instructions, direction) = get_next_instruction instructions
    case direction of
        '<' -> do_one_rock new_instructions (move_left points columns) columns -- ebc fix move left and equivalents
        '>' -> do_one_rock new_instructions (move_right points columns) columns
        '|' -> do
            let (new_points, rc) = move_down points columns
            if rc
            then do_one_rock new_instructions new_points columns
            else (new_instructions, Data.Set.union new_points columns)
        _ -> error ("should have matched one of previous chars " ++ show direction)


get_max_height :: Set (Int, Int) -> Int
get_max_height columns = maximum heights
    where heights = Data.Set.map (\(_, y) -> y) columns

get_next_instruction :: (String, String) -> ((String, String), Char)
get_next_instruction (done, to_do) = do
    if length to_do > 0
    then do
        let direction = head to_do
        ((done ++ [direction], drop 1 to_do), direction)
    else do
        let direction = head done
        (([direction], drop 1 done), direction)


add_downs :: String -> String
add_downs instructions = do
    let downs = repeat '|'
    interleave instructions downs

interleave :: [a] -> [a] -> [a]
interleave (a1:a1s) (a2:a2s) = a1:a2:interleave a1s a2s
interleave _        _        = []


wall_clash_ok :: Set (Int, Int) -> Bool
wall_clash_ok points = do
    let xs = Data.Set.map (\(x, _) -> x) points
    (1 <= minimum xs) && (maximum xs <= 7)

-- returns set of points, whether or not they successfully moved
move_left :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
move_left points columns = do
    let new_points = Data.Set.map (\(x, y) -> (x-1, y)) points
    if (length (Data.Set.intersection new_points columns) == 0) && (wall_clash_ok new_points)
    then new_points
    else points

-- returns set of points, whether or not they successfully moved
move_right :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
move_right points columns = do
    let new_points = Data.Set.map (\(x, y) -> (x+1, y)) points
    if (length (Data.Set.intersection new_points columns) == 0) && (wall_clash_ok new_points)
    then new_points
    else points

-- returns set of points, and if they successfully moved or not
move_down :: Set (Int, Int) -> Set (Int, Int)-> (Set (Int, Int), Bool)
move_down points columns = do
    let new_points = Data.Set.map (\(x, y) -> (x, y-1)) points
    if (length (Data.Set.intersection new_points columns) == 0) && (wall_clash_ok new_points)
    then (new_points, True)
    else (points, False)

get_next_shape :: Int -> Int -> Set (Int, Int)
get_next_shape rock_number max_height=
    case (rock_number `mod` 5) of
        1 -> get_horizontal_line max_height
        2 -> get_plus max_height
        3 -> get_l max_height
        4 -> get_vertical_line max_height
        0 -> get_square max_height
        _ -> error "should be impossible to hit here"

get_horizontal_line :: Int -> Set (Int, Int)
get_horizontal_line max_height =
    fromList [(3, max_height + 4), (4, max_height + 4), (5, max_height + 4), (6, max_height + 4)]

get_plus :: Int -> Set (Int, Int)
get_plus max_height =
    fromList [(3, max_height + 5), (4, max_height + 5), (5, max_height + 5), (4, max_height + 4), (4, max_height + 6)]

get_l :: Int -> Set (Int, Int)
get_l max_height =
    fromList [(3, max_height + 4), (4, max_height + 4), (5, max_height + 4), (5, max_height + 5), (5, max_height + 6)]

get_vertical_line :: Int -> Set (Int, Int)
get_vertical_line max_height =
    fromList [(3, max_height + 4), (3, max_height + 5), (3, max_height + 6), (3, max_height + 7)]

get_square :: Int -> Set (Int, Int)
get_square max_height =
    fromList [(3, max_height + 4), (3, max_height + 5), (4, max_height + 4), (4, max_height + 5)]