module RunGame where

import Data.Char
import System.Random
import UserInterface
import Debug.Trace


  -- | The interface to the students' implementation.
data Interface = Interface
  {   iBoard      :: Board
    , iThrowDice  :: StdGen -> (Int, Int, Int, Int)
    , iMakeMove   :: (Int, Int) -> Board -> Board
    , iLegalMove  :: Board -> Move -> Bool
    , iCreateMove :: Color -> Int -> Int -> Move
  }

-- | Runs a game given an implementation of the interface.
runGame :: Interface -> IO ()
runGame i =
  do putStrLn "Welcome to Haskgammon."
     g <- newStdGen
     gameLoop i g (iBoard i) (Player White Human) (Player Black Human)

-- | Play until the guest player is bust or chooses to stop.
--gameLoop :: Interface -> StdGen -> Board -> Player -> Player -> IO ()
--gameLoop i g board p1 p2 =
  --do putStrLn ( "Current board: ")
--     printPoints (points board)
  --TODO Determine when player has won!
     --if iGameOver i guest
      -- then finish i deck guest
       -- else do putStrLn "Draw another card? [y]"
       --         yn <- getLine
       --         if null yn || not (map toLower yn == "n")
       --           then do let (deck', guest') = iDraw i deck guest
       --                   gameLoop i deck' guest'
       --           else finish i deck guest
         -- do putStrLn "Roll dice? [y]"
         --    yn <- getLine
         --    putStrLn yn
            -- if null yn || not (map toLower yn == "n")
            --   then let thrownDice = iThrowDice g
            --       do  putStrLn"You got following values: " ++ show (throwDice)
            --       do  putStrLn"Enter the move you want to make: "
            --       input <- getLine
            --       val <- map digitToInt (splitOn " " input)
            --       moveToMake <- iCreateMove (color p1) (val!!0 - val!!1) val!!0
            --       if iLegalMove moveToMake
            --         then iMakeMove
            --       else do putStrLn "Not an allowed move!"


-- | Display the bank's final score and the winner.
-- finish :: Interface -> Hand -> Hand -> IO ()
-- finish i deck guest =
--   do putStrLn ("The bank's final score: " ++ show (iValue i bank))
--      putStrLn ("Winner: " ++ show (iWinner i guest bank))
--   where
--     bank = iPlayBank i deck

printPoints :: [Point] -> IO ()
printPoints [] = return ()
printpoint (x:xs) = trace("printPoints" ++ show (x)) printCheckers(checkers x) >> (printPoints xs)

printCheckers :: [Checker] -> IO ()
printCheckers [] = return ()
printCheckers (x:xs) = printChecker x >> (printCheckers xs)

printChecker :: Checker -> IO ()
printChecker c = putStrLn (show c)

-- | Show a backgammon board
-- printBoard :: Board -> IO ()


