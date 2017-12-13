module Haskgammon where

import Test.QuickCheck
import System.Random
import Debug.Trace

import BackgammonTypes
import TextUI

-- | init start config for Backgammon board
--   This is how it looks before any move has been made
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

-- | Helper method for setting up a init start config
createCheckers :: Int -> Color -> [Checker]
createCheckers n c = helpercheckers [] n c

-- | recursive adds as many checkers to a list as been specified by the input
helpercheckers :: [Checker] -> Int -> Color -> [Checker]
helpercheckers l 0 _ = l
helpercheckers l n c = helpercheckers (l ++ [Checker(c)]) (n-1) c

-- | perform two thrown dice. If the two throws are the same double their value
throwDice :: StdGen -> (Int, Int, Int, Int)
throwDice g = if f == s
                then (f,s,f,s)
              else
                (f,s,0,0)
               where (r, f, s) = throwHelper g

-- | helper function to throwDice for generating random numbers
throwHelper :: StdGen -> (Int, Int, Int)
throwHelper g = (n1, n2, n3)
    where (n1, g1) = (randomR (1, 6) g)
          (n2, g2) = (randomR (1, 6) g1)
          (n3, g3) = (randomR (1, 6) g2)

-- | perform a move on the backgammon board
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

-- | adds a checker to a users bar. This happens when a checker gets taken out
addToBar :: Board -> Color -> Board
addToBar (Board p wh bl) White =  Board p (wh + 1) bl
addToBar (Board p wh bl) Black =  Board p wh (bl + 1)

-- | helper function to invert a color to the opposite value
invertColor :: Color -> Color
invertColor White = Black
invertColor Black = White

-- | check if the move a player want to make is allowed to do
legalMove :: Board -> Move -> Bool
legalMove b (Move c (start, end))
    | not (insideBoard m) && bearOff b c = legalBearOffMove b m
    | isEmpty (getPoint b end) = True
    | hasAny c (getPoint b end) = True
    | c == White && blot Black (getPoint b end) = True
    | c == Black && blot White (getPoint b end) = True
    | otherwise = False
    where m = Move c (start, end)

-- | Used in make move to remove a checker from a point
removeFromPoint :: Int -> Board -> Board
removeFromPoint 0 (Board (x:xs) wh bl) =
  (Board ([popChecker x] ++ xs) wh bl)
removeFromPoint n (Board (x:xs) wh bl) =
  (Board ([x] ++ (points (removeFromPoint (n-1) (Board xs wh bl )))) wh bl)

-- | helper function. Take in a point and removes a checker
popChecker :: Point -> Point
popChecker (Point c) = Point(tail c)

-- | Adds a checker to a point
addToPoint :: Color -> Int -> Board -> Board
addToPoint c 0 (Board (x:xs) wh bl) =
  (Board ([appendChecker c x] ++ xs) wh bl)
addToPoint c n (Board (x:xs) wh bl) =
  (Board ([x] ++ (points (addToPoint c (n-1) (Board xs wh bl))))wh bl)

-- | Helper function. Take a point and adds a checker to it.
appendChecker :: Color -> Point -> Point
appendChecker c (Point x) = Point(x ++ [Checker(c)])

-- | Return all allowed moves for a player with a given dice roll.
-- NOTE: Never used, intentionally ment if features for a bot would have been created
allLegalMoves :: Board -> Color -> Int -> [Move]
allLegalMoves b c diceroll = filter (legalMove b)
        (allPossibleMoves b c diceroll)

-- | Return all possible moves for a player (including illegal moves)
-- NOTE: Never used, intentionally ment if features for a bot would have been created
allPossibleMoves :: Board -> Color -> Int -> [Move]
allPossibleMoves b c d = map (createMove c d)
        (filter (\x -> hasAny c  (getPoint b x)) [0..23])

-- | Helper function to retrieve a point at a specific position
getPoint :: Board -> Int -> Point
getPoint (Board p _ _) i = p !! i

-- | Check if point contains any checkers
isEmpty :: Point -> Bool
isEmpty (Point []) = True
isEmpty _ = False

-- | Check if a point contains any checkers
hasAny :: Color -> Point -> Bool
hasAny Black (Point ((Checker Black):_)) = True
hasAny Black _ = False
hasAny White (Point ((Checker White):_)) = True
hasAny White _ = False

-- | Checks if there is only one checker contained in a certain point
-- i.e. the checker being a blot
blot :: Color -> Point -> Bool
blot c (Point checkers) = ((length checkers) == 1) && (color (head checkers)) == c

-- | validate that a given move is inside a board.
insideBoard :: Move -> Bool
insideBoard (Move _ (_, end)) = (end >= 0) && (end <= 23)

-- | Check if a player's all checkers in located in the last house
-- if so, the player has other rules
bearOff :: Board -> Color -> Bool
bearOff b White = length (filter (\x -> hasAny White (getPoint b x)) [0..17]) == 0
bearOff b Black = length (filter (\x -> hasAny Black (getPoint b x)) [6..23]) == 0

-- | Checks if the checker is specified is the last in checker in the players house.
-- If so, it's possible to go out with the checker even if the move exeeds the
-- backgammon board
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

-- | Creates a move type to be used in other functions
createMove :: Color -> Int -> Int -> Move
createMove Black jump start = (Move Black (start, (start - jump)))
createMove White jump start = (Move White (start, (start + jump)))

-- | checks if a player has won
hasWon :: Color -> Board -> Bool
hasWon c b =  length (filter (hasAny c) (points b)) ==  0 && noBar b c

-- | validates that a player has no checkers on the bar
noBar :: Board -> Color -> Bool
noBar b White = (whiteBar b) == 0
noBar b Black = (blackBar b) == 0

-- | Takes a dice roll and remove the value of the move from the dice
decDice :: (Int,Int,Int,Int) -> Move-> (Int,Int,Int,Int)
decDice (a,b,c,d) (Move White (start,end))
  | (end - start) == a = (b,c,d,0)
  | (end - start) == b = (c,d,0,a)
  | (end - start) == c = (d,0,a,b)
  | (end - start) == d = (0,a,b,c)
  | otherwise = (a,b,c,d)

-- checks if a player has more moves to make
hasMadeAllMoves :: (Int,Int,Int,Int) -> Bool
hasMadeAllMoves (a,b,c,d) = (a,b,c,d) == (0,0,0,0)

-- | used by the GUI from wrapping the game logic all togeher.
-- The crucial method for making the game available to play
doTurn :: Move -> GameState -> StdGen -> GameState
doTurn (Move color (start,end)) (GameState win p1 p2 b c d) gen
  | legalMove b m == False =
    (GameState win p1 p2 b "not legal move! choose another move!" d)
  | not (hasMadeAllMoves diceAfter) =
    (GameState (hasWon color boardAfter) p1 p2 boardAfter "make another move" diceAfter)
  | otherwise =
    (GameState (hasWon color boardAfter) p2 p1 boardAfter "Next player turn!" (throwDice gen))
    where m = (Move color (start,end))
          g = (GameState win p1 p2 b c d)
          boardAfter = makeMove (start,end) b
          diceAfter = decDice d m


implementation = Interface {
      iHandleMove = doTurn
     ,iThrowDice = throwDice
}

main = setup implementation
  (GameState False (Player White Human) (Player Black Human) (stdStart) ("Start") (1,1,1,1))



--
--
--
--
