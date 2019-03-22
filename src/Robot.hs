module Robot where
import Control.Monad
import Data.Char

data Position = Position Int Int Direction deriving (Show, Eq)
data Spin = LeftTurn | RightTurn deriving (Show, Eq)
data Direction = North | East | South | West deriving (Enum, Show, Eq)

changeDirection :: Direction -> Spin -> Direction
changeDirection North LeftTurn = West
changeDirection West RightTurn = North
changeDirection d LeftTurn = pred d
changeDirection d RightTurn = succ d

move :: Position -> Maybe Position
move (Position x y d) = let movedTo x y = Just (Position x y d)
    in case d of 
    North -> movedTo x (y+1)
    East  -> movedTo (x+1) y
    South -> movedTo x (y-1)
    West  -> movedTo (x-1) y

turn :: Position -> Spin -> Position
turn (Position x y d) spin = Position x y (changeDirection d spin)

consume :: Char -> Position -> Maybe Position
consume i pos@(Position x y d) = 
    case i of 
    'L' -> Just (turn pos LeftTurn)
    'R' -> Just (turn pos RightTurn)
    'M' -> move pos
    _   -> Nothing

robot :: String -> Maybe Position -> Maybe Position
robot xs start = start >>= foldr (>=>) return (map consume xs)

parsePosition :: String -> Maybe Position
parsePosition xs = 
    let x = read $ words xs !! 0 :: Int
        y = read $ words xs !! 1 :: Int
        d = parseDirection $ (words xs) !! 2
    in Just (Position x y d)

parseDirection :: String -> Direction
parseDirection d = case d of
    "N" -> North
    "E" -> East
    "S" -> South
    "W" -> West