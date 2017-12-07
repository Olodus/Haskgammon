module UserInterface where

import Data.Char
import System.Random


-- | The interface to the students' implementation.
data Interface = Interface
  { iEmpty    :: Board
  , iFullDeck :: Hand
  , iValue    :: Hand -> Integer
  , iGameOver :: Hand -> Bool
  , iWinner   :: Hand -> Hand -> Player
  , iDraw     :: Hand -> Hand -> (Hand, Hand)
  , iPlayBank :: Hand -> Hand
  , iShuffle  :: StdGen -> Hand -> Hand
  }

-- | A type of players.
data Player = Human | Bot
              deriving (Show, Eq)

-- | Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i =
  do putStrLn "Welcome to Haskgammon."
     g <- newStdGen
     gameLoop i (iShuffle i g (iFullDeck i)) (iEmpty i)

-- | Play until the guest player is bust or chooses to stop.
gameLoop :: Interface -> Board -> Player -> Player -> IO ()
gameLoop i deck guest =
  do putStrLn ("Your current score: " ++ show (iValue i guest))
     if iGameOver i guest
       then finish i deck guest
       else do putStrLn "Draw another card? [y]"
               yn <- getLine
               if null yn || not (map toLower yn == "n")
                 then do let (deck', guest') = iDraw i deck guest
                         gameLoop i deck' guest'
                 else finish i deck guest

-- | Display the bank's final score and the winner.
finish :: Interface -> Hand -> Hand -> IO ()
finish i deck guest =
  do putStrLn ("The bank's final score: " ++ show (iValue i bank))
     putStrLn ("Winner: " ++ show (iWinner i guest bank))
  where
    bank = iPlayBank i deck


-- | Show a backgammon board
printBoard :: Board -> IO ()
