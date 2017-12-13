module BackgammonTypes where

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

data Move = Move { moveColor :: Color, move :: (Int, Int) }
            deriving (Eq, Show)

data GameState = GameState {gameOver :: Bool, activePlayer :: Player, nextPlayer :: Player, board :: Board, dialog :: String, dice :: (Int, Int, Int, Int)}

data Player = Player { playerColor :: Color, playerType :: PlayerType}
                deriving (Show, Eq)

data PlayerType = Human | Bot
              deriving (Show, Eq)
