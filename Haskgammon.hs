module Haskgammon where

import Test.QuickCheck
import System.Random

data Board = Board { points :: [Point], whiteBar :: Int, blackBar :: Int}
             deriving (Eq, Show)

data Point = Point { checkers :: [Checker] }
             deriving (Eq, Show)

data Checker = Checker { color :: Color }
               deriving (Eq, Show)

data Color = White | Black
             deriving (Eq, Show)

data Move = Move { playerColor :: Color, move :: (Int, Int) }
            deriving (Eq, Show)


stdStart :: Board
stdStart = (Board [Point( createCheckers 2 White),
            Point([]), Point([]), Point([]), Point([]),
            Point(createCheckers 5 Black),
            Point([]),
            Point(createCheckers 3 Black),
            Point([]), Point([]), Point([]),
            Point(createCheckers 5 White),
            Point(createCheckers 5 Black),
            Point([]), Point([]), Point([]),
            Point(createCheckers 3 White),
            Point([]),
            Point(createCheckers 5 White),
            Point([]), Point([]), Point([]), Point([]),
            Point(createCheckers 2 Black)] 0 0)

boardWhite :: Board
boardWhite =  (Board [
            Point([]), Point([]), Point([]), Point([]),
            Point(createCheckers 5 Black),
            Point([]),
            Point(createCheckers 3 Black),
            Point([]), Point([]), Point([]),
            Point([]), Point([]), Point([]), Point([]), Point([]),
            Point(createCheckers 5 Black),
            Point([]),Point(createCheckers 1 White),

            Point(createCheckers 1 White), Point(createCheckers 1 White),
            Point(createCheckers 3 White),
            Point( createCheckers 2 White),
            Point(createCheckers 5 White),
            Point(createCheckers 5 White)] 0 0)


boardBlack :: Board
boardBlack =  (Board [
            Point(createCheckers 1 Black), Point(createCheckers 1 Black),
            Point(createCheckers 3 Black),
            Point( createCheckers 2 Black),
            Point(createCheckers 5 Black),
            Point(createCheckers 5 Black),

            Point([]), Point([]), Point([]), Point([]),
            Point(createCheckers 5 White),
            Point([]),
            Point(createCheckers 3 White),
            Point([]), Point([]), Point([]),
            Point([]), Point([]), Point([]), Point([]), Point([]),
            Point(createCheckers 5 White),
            Point([]),Point(createCheckers 1 White)] 0 0)
createCheckers :: Int -> Color -> [Checker]
createCheckers n c = helpercheckers [] n c

helpercheckers :: [Checker] -> Int -> Color -> [Checker]
helpercheckers l 0 _ = l
helpercheckers l n c = helpercheckers (l ++ [Checker(c)]) (n-1) c

throwDice :: StdGen -> (Int, Int, Int, Int)
throwDice g = if f == s
                then (f,s,f,s)
              else
                (f,s,0,0)
               where (r, f, s) = throwHelper g

throwHelper :: StdGen -> (Int, Int, Int)
throwHelper g = (n1, n2, n3)
    where (n1, g1) = (randomR (1, 6) g)
          (n2, g2) = (randomR (1, 6) g1)
          (n3, g3) = (randomR (1, 6) g2)


makeMove :: (Int, Int) -> Board -> Board
makeMove (start, end) board = addToPoint c end newboard
        where newboard = removeFromPoint start board
              c = color (head (checkers ((points board) !! start)))

legalMove :: Board -> Move -> Bool
legalMove b (Move c (start, end))
    | not (insideBoard m) && bearOff b c = legalBearOffMove m
    | isEmpty (getPoint b end) = True
    | hasAny c (getPoint b end) = True
    | c == White && blot Black (getPoint b end) = True
    | c == Black && blot White (getPoint b end) = True
    | otherwise = False
    where m = Move c (start, end)

removeFromPoint :: Int -> Board -> Board
removeFromPoint 0 (Board (x:xs) wh bl) = (Board ([popChecker x] ++ xs) wh bl)
removeFromPoint n (Board (x:xs) wh bl) = (Board ([x] ++ (points (removeFromPoint (n-1) (Board xs wh bl )))) wh bl)

popChecker :: Point -> Point
popChecker (Point c) = Point(tail c)

addToPoint :: Color -> Int -> Board -> Board
addToPoint c 0 (Board (x:xs) wh bl) = (Board ([appendChecker c x] ++ xs) wh bl)
addToPoint c n (Board (x:xs) wh bl) = (Board ([x] ++ (points (addToPoint c (n-1) (Board xs wh bl))))wh bl)

appendChecker :: Color -> Point -> Point
appendChecker c (Point x) = Point(x ++ [Checker(c)])

-- | Return all allowed moves for a player with a given dice roll
allLegalMoves :: Board -> Color -> Int -> [Move]
allLegalMoves b c diceroll = filter (legalMove b) (allPossibleMoves b c diceroll)

-- | Return all possible moves for a player (including illegal moves)
--  with a given dice roll
allPossibleMoves :: Board -> Color -> Int -> [Move]
allPossibleMoves b c d = map (createMove c d) (filter (\x -> hasAny c  (getPoint b x)) [0..23])
-- map (createMove c d2) (filter (hasAny c (getPoint b)) [0..23])
-- All double-jump moves too
-- I also need to be able to represent these as Move (recursive type?)
-- also double Dicerolls...?
--
-- possibleHelper :: Board -> Color -> Int -> [Move]

getPoint :: Board -> Int -> Point
getPoint (Board p _ _) i = p !! i

-- | Check if point contains any checkers
isEmpty :: Point -> Bool
isEmpty (Point []) = True
isEmpty _ = False

-- Is it possible to write these Black and White true as one?
hasAny :: Color -> Point -> Bool
hasAny Black (Point ((Checker Black):_)) = True
hasAny Black _ = False
hasAny White (Point ((Checker White):_)) = True
hasAny White _ = False
--
-- hasAnyAt :: Board -> Color -> Int -> Bool


-- | Checks if there is only one checker contained in a certain point
blot :: Color -> Point -> Bool
blot c (Point checkers) = ((length checkers) == 1) && (color (head checkers)) == c


insideBoard :: Move -> Bool
insideBoard (Move _ (_, end)) = (end >= 0) && (end <= 23)

-- | Check if a player's all checkers in located in the last house
bearOff :: Board -> Color -> Bool
bearOff b White = length (filter (\x -> hasAny White (getPoint b x)) [0..17]) == 0
bearOff b Black = length (filter (\x -> hasAny Black (getPoint b x)) [6..23]) == 0

legalBearOffMove :: Move -> Bool
legalBearOffMove (Move c (start, end)) = True

-- No this is not how we should do it. Direction should probably be a type class / function you can do on a Board. Somethins like color Board executes in one way or another... though that is just a function... I dont know... Monads man monads...
--getDirection :: Color -> (Int -> Int)
--getDirection White = (\x, y -> x + y)
--getDirection Black = (\x, y -> x - y)
--Like this?...
createMove :: Color -> Int -> Int -> Move
createMove Black jump start = (Move Black (start, (start - jump)))
createMove White jump start = (Move White (start, (start + jump)))
