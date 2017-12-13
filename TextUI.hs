module TextUI where

import Data.Char
import System.Random
import Debug.Trace
import Test.QuickCheck
import System.Random
import System.IO

import BackgammonTypes

data Interface = Interface
  { iHandleMove :: Move -> GameState -> StdGen -> GameState
  , iThrowDice :: StdGen -> (Int, Int, Int, Int) }

setup :: Interface -> GameState -> IO ()
setup (gameLogic) (GameState win p1 p2 b c _) = do
     putStrLn "Welcome to Backgammon!!! \n \n"
     gen <- newStdGen --(mkStdGen ran)
     let dice = (iThrowDice gameLogic) gen
     gameLoop (gameLogic) (GameState win p1 p2 b c dice) gen

gameLoop :: Interface -> GameState -> StdGen -> IO ()
gameLoop (gameLogic) (GameState win p1 p2 b c d) gen 
     | (gameOver state) == True = do putStrLn ((show (playerColor (activePlayer state))) ++ " won!!!")
     | otherwise = do 
          putStrLn (showTurn state)
          start <- getLine
          putStr "And where move end: "
          end <- getLine
          let move = (parseMove (playerColor p1) start end)
          let newState = ((iHandleMove gameLogic) move state gen)
          gameLoop (gameLogic) newState gen
     where state = (GameState win p1 p2 b c d)
          

showTurn :: GameState -> String
showTurn (GameState True p1 _ _ _ _) = ""
showTurn (GameState False p1 p2 b c d) = (show (playerColor p1)) ++ "'s turn -- " ++ c ++ "\n" ++ (showFirstRow b) ++ "\n" ++ (showSecondRow b) ++ "\n \n" ++ (showDice d) ++ "\n \n Write where move start: "

showFirstRow :: Board -> String
showFirstRow (Board p _ _) = firstRowHelper (take 12 p) 

firstRowHelper :: [Point] -> String
firstRowHelper [] = ""
firstRowHelper ((Point c):xs) 
     | length c == 0 = "0  " ++ (firstRowHelper xs)
     | (color (head c)) == White = "W" ++ (show (length c)) ++ " " ++ (firstRowHelper xs)
     | (color (head c)) == Black = "B" ++ (show (length c)) ++ " " ++ (firstRowHelper xs)
     | otherwise = "0  " ++ (firstRowHelper xs)



showSecondRow :: Board -> String
showSecondRow (Board p _ _) = secondRowHelper (drop 12 p)

secondRowHelper :: [Point] -> String
secondRowHelper [] = ""
secondRowHelper ((Point c):xs) 
     | length c == 0 = "0  " ++ (secondRowHelper xs)
     | (color (head c)) == White = "W" ++ (show (length c)) ++ " " ++ (secondRowHelper xs)
     | (color (head c)) == Black = "B" ++ (show (length c)) ++ " " ++ (secondRowHelper xs)
     | otherwise = "0  " ++ (secondRowHelper xs)

showDice :: (Int, Int, Int, Int) -> String
showDice d = (show d)

parseMove :: Color -> String -> String -> Move
parseMove c start end = (Move c (digitToInt (head start), digitToInt (head end)))
