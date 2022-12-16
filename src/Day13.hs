module Day13 where


day13_part1 :: [String] -> Int
day13_part1 xs = do
    compare_pairs (parse xs) 1


day13_part2 :: [String] -> Int
day13_part2 ys = do
    let xs = parse2 ys
    let pa = PktL (parse_packet "[[2]]")
    let pb = PktL (parse_packet "[[6]]")
    ((compare_to_list pa xs) + 1) * ((compare_to_list pb xs) + 2)


parse2 :: [String] -> [Packet]
parse2 xs =
    case xs of
        x:xs ->
            if length x /= 0
            then (PktL (parse_packet x)):(parse2 xs)
            else parse2 xs
        [] -> []


compare_to_list :: Packet -> [Packet] -> Int
compare_to_list p xs =
    case xs of
        x:xs ->
            (compare_pkt x p) + (compare_to_list p xs)
        [] -> 0


data Packet = PktI Int | PktL [Packet] deriving (Show)

compare_pairs :: [(Packet, Packet)] -> Int -> Int
compare_pairs xs n =
    case xs of
        x:xs -> do
            let (left, right) = x
            n*(compare_pkt left right) + (compare_pairs xs (n+1))
        [] -> 0


-- return 0 if failed (R < L), +1 if correct (L < R), -1 for continue (R == L)
compare_pkt :: Packet -> Packet -> Int
compare_pkt left right =
    case left of
        PktL xs ->
            case right of
                PktL ys ->
                    if
                        | (length xs == 0) && (length ys > 0) -> 1
                        | (length ys == 0) && (length xs > 0) -> 0
                        | (length xs == 0) && (length ys == 0) -> -1
                        | otherwise ->
                            if (compare_pkt (head xs) (head ys)) /= -1
                            then (compare_pkt (head xs) (head ys))
                            else (compare_pkt (PktL (tail xs)) (PktL (tail ys)))
                PktI _i2 ->
                    compare_pkt left (PktL [right])
        PktI ileft ->
            case right of
                PktL _ys ->
                    compare_pkt (PktL [left]) right
                PktI iright ->
                    if ileft > iright
                    then 0
                    else
                        if ileft < iright
                        then 1
                        else -1


parse_packet :: String -> [Packet]
parse_packet xs =
    case xs of
        x:xs ->
            if x == '['
            then do
                let inside = find_inside xs 1
                let remaining = drop ((length inside)+1) xs
                (PktL (parse_packet inside)):parse_packet remaining
            else
                if x == ','
                then
                    parse_packet xs
                else do
                    let int :: Int = read (find_int (x:xs))
                    let remaining = drop (length (find_int (x:xs))) (x:xs)
                    case remaining of
                        x:xs ->
                            if x == ','
                            then
                                (PktI int):(parse_packet xs)
                            else
                                (PktI int):(parse_packet remaining)
                        [] -> (PktI int):(parse_packet remaining)
        [] -> []


find_inside :: String -> Int -> String
find_inside xs n =
    case xs of
        x:xs ->
            if x == '['
            then x:(find_inside xs (n+1))
            else
                if x == ']' && n /= 1
                then x:(find_inside xs (n-1))
                else
                    if x == ']' && n == 1
                    then []
                    else x:(find_inside xs (n))
        [] -> error "finding substring failed"
                

find_int :: String -> String
find_int xs =
    case xs of
        x:xs ->
            if x == ']'
            then error "x is bad"
            else
                if x == ','
                then []
                else x:(find_int xs)
        [] -> []


parse :: [String] -> [(Packet, Packet)]
parse xs =
    case xs of
        _:_:ys -> do
            let pkt = (head (parse_packet (head xs)), head (parse_packet (head (tail xs))))
            case ys of
                _:ys -> pkt:parse ys
                [] -> [pkt]
        [] -> []
        _ -> error "should have matched previosu cases"


