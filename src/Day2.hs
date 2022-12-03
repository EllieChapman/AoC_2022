module Day2 where


-- ["A Y","B X","C Z"]
day2_part1 :: [String] -> Int
day2_part1 xs =
    case xs of
        x:xs -> pair_to_score (string_to_pair x) + day2_part1 xs
        [] -> 0
    -- case xs of
    --     x:xs -> pair_to_score (string_to_pair x) : day2_part1 xs
    --     [] -> []


string_to_pair :: String -> (Char, Char)
string_to_pair xs =
    (head xs, head (tail (tail xs)))

pair_to_score :: (Char, Char) -> Int
pair_to_score x = do
    let (a, b) = x
    let rb = convert_to_same b
    choice_score rb + win_score a rb

choice_score :: Char -> Int
choice_score x =
    case x of
        'A' -> 1
        'B' -> 2
        'C' -> 3
        _ -> error "trying to get score for undefined char"


convert_to_same :: Char -> Char
convert_to_same x =
    case x of
        'X' -> 'A'
        'Y' -> 'B'
        'Z' -> 'C'
        _ -> error [x]


win_score :: Char -> Char -> Int
win_score a b =
    case (a, b) of
        ('A', 'A') -> 3
        ('A', 'B') -> 6
        ('A', 'C') -> 0
        ('B', 'A') -> 0 
        ('B', 'B') -> 3
        ('B', 'C') -> 6
        ('C', 'A') -> 6
        ('C', 'B') -> 0
        ('C', 'C') -> 3
        (_, _) -> error "comparing impossible pair"



day2_part2 :: [String] -> Int
day2_part2 xs =
    case xs of
        x:xs -> pair_to_score2 (string_to_pair x) + day2_part2 xs
        [] -> 0


pair_to_score2 :: (Char, Char) -> Int
pair_to_score2 x = do
    let (a, b) = x
    let my_choice = my_play a b
    choice_score my_choice + enforced_score b


-- lose draw win
my_play :: Char -> Char -> Char
my_play a b =
    case (a, b) of
        ('A', 'X') -> 'C'
        ('A', 'Y') -> 'A'
        ('A', 'Z') -> 'B'
        ('B', 'X') -> 'A'
        ('B', 'Y') -> 'B'
        ('B', 'Z') -> 'C'
        ('C', 'X') -> 'B'
        ('C', 'Y') -> 'C'
        ('C', 'Z') -> 'A'
        (_, _) -> error "comparing impossible pair"

enforced_score :: Char -> Int
enforced_score a = 
    case a of
        'X' -> 0
        'Y' -> 3
        'Z' -> 6
        _ -> error "iuhui"
