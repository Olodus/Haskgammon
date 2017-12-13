module TextUI where

import Data.Char
import System.Random
import Debug.Trace
import Test.QuickCheck
import System.Random

import BackgammonTypes

data Interface = Interface
  { iHandleMove :: Move -> GameState -> StdGen -> GameState }

setup (Interface gameLogic) initBoard ran = do
     putLine "Welcome to Backgammon"
     gen <- mkStdGen ran
     gameLoop gameLogic initBoard gen

gameLoop :: (Move -> GameState -> StdGen -> GameState) -> StdGen -> GameState
gameLoop gameLogic gen state 
     | (gameOver state) == True = do putLine (show (color (p1 state))) ++ " won!!!"
     | otherwise = do 
          putLine (showTurn initBoard)
          start <- getLine
          putLine "And where move end: "
          end <- getLine
          move <- (parseMove start end)
          newState <- gameLogic move state gen
          gameLoop gameLogic gen newState
          

showTurn :: GameState -> String
showTurn (GameState True p1 _ _ _ _) = ""
showTurn (GameState False p1 p2 b c d) = (show (color p1)) ++ "'s turn -- " ++ c ++ "\n" ++ (showFirstRow b) ++ "\n" ++ (showSecondRow b) ++ "\n \n" ++ (showDice d) ++ "\n \n Write where move start: "

showFirstRow :: Board -> String
showFirstRow b = "dofvnd"

showSecondRow :: Board -> String
showSecondRow b = "oindfv"

showDice :: (Int, Int, Int, Int) -> String
showDice d = "oidifvoi"
