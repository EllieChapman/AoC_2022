module Day17 where

import Data.Set (Set, map, fromList, toList)
import Data.List (sort)

day17_part1 :: [String] -> Int
day17_part1 xs = do
    let modified_line = add_downs (head xs)
    let instructions = ("", modified_line) -- done and todo strings
    simulate_n_rocks instructions 1 2022 [[0],[0],[0],[0],[0],[0],[0]]

-- to map height of floor, meed to keep track of max height of all 7 columns and that's it?
    -- not quite, possible to go past and slide under
    -- instead, maybe represent each column as a list of heights which have a block in, will normally only check first few elements

-- need to track column which is highest for shape spawning

-- need way to represent shapes, and to be able to apply < > and down to them
    -- maybe just set of (x, y) points? Always know what they are, as x is fixed and y is fixed + heighest column
    -- easy to apply left right or down to set of points, can map correct function over them

-- testing move
    -- apply move map funciton, get where shape would be if moved
    -- check if any points are already containing rock or edge
    -- if ok return new set of points plus ok RC
    -- if not ok, return old set, plus diff RC for if sideways move failed vs down move failed, as only down means finished

-- need an add to floor function, to use after a down movement failed. This should also update max height/what highest column is


-- eventually will return max height after n rocks simulated. Start will passing in 1 and rocks to simukate
simulate_n_rocks :: (String, String) -> Int -> Int -> [[Int]] -> Int
simulate_n_rocks instructions rock_number rocks_to_simulate columns =
    if rock_number <= rocks_to_simulate
    then do
        let rock = get_next_shape rock_number (get_max_height columns)
        let (new_instructions, new_columns) = do_one_rock instructions rock columns
        simulate_n_rocks new_instructions (rock_number + 1) rocks_to_simulate new_columns
    else get_max_height columns
    -- else error ("columsn " ++ show columns ++ show (get_max_height columns))
    -- if rock_number <= rocks to simukate do a rock, else return max height
    -- do a rock, recurse and pass through updated instauctions, rock_number, and columns. rocks_to_simulate is unchanged

    -- to do a rock 
        -- get rock using get next rock call giving it current rock id and max_height
        -- call do_one_rock, passing in instructions, rock points, columns

-- returns updated instructions tuple and updated columns once rock is landed
do_one_rock :: (String, String) -> Set (Int, Int) -> [[Int]] -> ((String, String), [[Int]])
do_one_rock instructions points columns = do
    let (new_instructions, direction) = get_next_instruction instructions
    case direction of
        '<' -> do_one_rock new_instructions (move_left points columns) columns
        '>' -> do_one_rock new_instructions (move_right points columns) columns
        '|' -> do
            let (new_points, rc) = move_down points columns
            if rc
            then do_one_rock new_instructions new_points columns
            else (new_instructions, create_floor new_points columns)
        _ -> error ("should have matched one of previous chars " ++ show direction)
    -- call get next instructions, store of char and updated instruction tuple
    -- depending on char, map correct function over set and look at returned bool
    -- if left or right recurse always
    -- if down and true recurse as it was a success
    -- if false, call create_floor and return new instryctions and columns

get_max_height :: [[Int]] -> Int
get_max_height columns =
    head (reverse (sort (Prelude.map head (Prelude.map reverse (Prelude.map sort columns)))))

create_floor :: Set (Int, Int) -> [[Int]] -> [[Int]]
create_floor points columns =
    case (toList) points of
        x:xs -> create_floor (fromList xs) (add_point_to_floor x columns)
        [] -> columns

add_point_to_floor :: (Int, Int) -> [[Int]] -> [[Int]]
add_point_to_floor (x, y) columns = do
    let new_column = toList (fromList (y:(head (drop (x-1) columns)))) -- ebc think ok
    (take (x-1) columns) ++ (new_column:(drop x columns))

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

clash :: Set (Int, Int) -> [[Int]] -> Bool
clash points columns =
    length (filter (\n -> n==True) (toList ((Data.Set.map (point_clash columns) points)))) > 0

-- return True if clashes
point_clash :: [[Int]] -> (Int, Int)-> Bool
point_clash columns (x, y) =
    if (x < 1) || (x > 7)
    then True
    else do
        let col = head (drop (x-1) columns)
        let len = length col
        if (length (fromList (y:col))) == len
        then True
        else False

-- returns set of points, whether or not they successfully moved
move_left :: Set (Int, Int) -> [[Int]] -> Set (Int, Int)
move_left points columns = do
    let new_points = Data.Set.map (\(x, y) -> (x-1, y)) points
    if clash new_points columns
    then points
    else new_points

-- returns set of points, whether or not they successfully moved
move_right :: Set (Int, Int) -> [[Int]] -> Set (Int, Int)
move_right points columns = do
    let new_points = Data.Set.map (\(x, y) -> (x+1, y)) points
    if clash new_points columns
    then points
    else new_points

-- returns set of points, and if they successfully moved or not
move_down :: Set (Int, Int) -> [[Int]] -> (Set (Int, Int), Bool)
move_down points columns = do
    let new_points = Data.Set.map (\(x, y) -> (x, y-1)) points
    if clash new_points columns
    then (points, False)
    else (new_points, True)

get_next_shape :: Int -> Int -> Set (Int, Int)
get_next_shape rock_number max_height=
    case (rock_number `mod` 5) of
        1 -> get_horizontal_line max_height
        2 -> get_plus max_height
        3 -> get_l max_height
        4 -> get_vertical_line max_height
        0 -> get_square max_height
        _ -> error "shoul dbe impossible to hit here"

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