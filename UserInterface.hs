module UserInterface where

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
