module Haskgammon where

import Test.QuickCheck
import System.Random
import Debug.Trace

import BackgammonTypes
import TextUI


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

createEmptyBoard :: Board
createEmptyBoard = (Board [
            Point([]), Point([]),
            Point([]),
            Point([]),
            Point([]),
            Point([]),

            Point([]), Point([]), Point([]), Point([]),
            Point([]),
            Point([]),
            Point([]),
            Point([]), Point([]), Point([]),
            Point([]), Point([]), Point([]), Point([]), Point([]),
            Point([]),
            Point([]),Point([])] 1 0)

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
makeMove (start, end) board =
        if (blot (invertColor c) p2)
          then (addToPoint c end
            (removeFromPoint start
            (removeFromPoint end
            (addToBar board (invertColor c)))))
        else
          addToPoint c end (removeFromPoint start board)
        where
              c = color (head (checkers p1))
              p1 = (points board) !! start
              p2 = (points board) !! end

addToBar :: Board -> Color -> Board
addToBar (Board p wh bl) White =  Board p (wh + 1) bl
addToBar (Board p wh bl) Black =  Board p wh (bl + 1)

invertColor :: Color -> Color
invertColor White = Black
invertColor Black = White

legalMove :: Board -> Move -> Bool
legalMove b (Move c (start, end))
    | not (insideBoard m) && bearOff b c = legalBearOffMove b m
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

-- | Checks if there is only one checker contained in a certain point
blot :: Color -> Point -> Bool
blot c (Point checkers) = ((length checkers) == 1) && (color (head checkers)) == c


insideBoard :: Move -> Bool
insideBoard (Move _ (_, end)) = (end >= 0) && (end <= 23)

-- | Check if a player's all checkers in located in the last house
bearOff :: Board -> Color -> Bool
bearOff b White = length (filter (\x -> hasAny White (getPoint b x)) [0..17]) == 0
bearOff b Black = length (filter (\x -> hasAny Black (getPoint b x)) [6..23]) == 0

legalBearOffMove :: Board -> Move -> Bool
legalBearOffMove b (Move White (17, end)) = True
legalBearOffMove b (Move White (start, end)) =
            if hasAny White (getPoint b (start- 1))
              then False
                else legalBearOffMove b (Move White ((start - 1), end))

legalBearOffMove b (Move Black (5, end)) = True
legalBearOffMove b (Move Black (start, end)) =
            if hasAny White (getPoint b (start + 1))
              then False
                else legalBearOffMove b (Move Black ((start + 1), end))

createMove :: Color -> Int -> Int -> Move
createMove Black jump start = (Move Black (start, (start - jump)))
createMove White jump start = (Move White (start, (start + jump)))

hasWon :: Color -> Board -> Bool
hasWon c b =  length (filter (hasAny c) (points b)) ==  0 && noBar b c

noBar :: Board -> Color -> Bool
noBar b White = (whiteBar b) == 0
noBar b Black = (blackBar b) == 0


decDice :: (Int,Int,Int,Int) -> Move-> (Int,Int,Int,Int)
decDice (a,b,c,d) (Move White (start,end))
  | (end - start) == a = (b,c,d,0)
  | (end - start) == b = (c,d,0,a)
  | (end - start) == c = (d,0,a,b)
  | (end - start) == d = (0,a,b,c)
  | otherwise = (a,b,c,d)

hasMadeAllMoves :: (Int,Int,Int,Int) -> Bool
hasMadeAllMoves (a,b,c,d) = (a,b,c,d) == (0,0,0,0)

doTurn :: Move -> GameState -> StdGen -> GameState
doTurn (Move color (start,end)) (GameState win p1 p2 b c d) gen  --(GameState True (Player White Human) (Player Black Human) (createEmptyBoard) ("Woop woop") (1,1,1,1))
  | legalMove b m == False = (GameState win p1 p2 b "not legal move! choose another move!" d)
  | not (hasMadeAllMoves diceAfter) = (GameState (hasWon color boardAfter) p1 p2 boardAfter "make another move" diceAfter)
  | otherwise = (GameState (hasWon color boardAfter) p2 p1 boardAfter "Next player turn!" (throwDice gen))
    where m = (Move color (start,end))
          g = (GameState win p1 p2 b c d)
          boardAfter = makeMove (start,end) b
          diceAfter = decDice d m


implementation = Interface {
      iHandleMove = doTurn
     ,iThrowDice = throwDice
}

main = setup implementation (GameState False (Player White Human) (Player Black Human) (stdStart) ("Start") (1,1,1,1)) 



--
--
--
--
