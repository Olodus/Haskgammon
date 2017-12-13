module UserInterface where

import Data.Char
import System.Random
import Debug.Trace
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import ThreepennyPages
import Test.QuickCheck
import System.Random

import BackgammonTypes

data Interface = Interface
  { iHandleMove :: Move -> GameState -> GameState }

canWidth,canHeight :: Num a => a
canWidth  = 50
canHeight = 100

setup (Interface hm) initBoard = startGUI defaultConfig (baseHTML initBoard hm)

baseHTML initBoard gameLogic window  = do
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     buttonBoard <- createCanvasBoard
     dialog  <- mkHTML "<i>Action</i>)="      -- The text "Action"
     p1      <- mkHTML "<b>White<b>"          -- Shows which player's turn it is
     p2      <- mkHTML "<b>Black<b>"          -- Shows the other player
     invis   <- mkPointList                   -- Only used in parsing state (never shown)
     b0 <- (mkButton (Just White) "2" 0)
     b1 <- (mkButton Nothing "" 1) 
     b2 <- (mkButton Nothing "" 2) 
     b3 <- (mkButton Nothing "" 3) 
     b4 <- (mkButton Nothing "" 4) 
     b5 <- (mkButton (Just Black) "5" 5) 
     b6 <- (mkButton Nothing "" 6) 
     b7 <- (mkButton (Just Black) "3" 7) 
     b8 <- (mkButton Nothing "" 8) 
     b9 <- (mkButton Nothing "" 9 ) 
     b10 <- (mkButton Nothing "" 10) 
     b11 <- (mkButton (Just White) "5" 11) 
     b12 <- (mkButton (Just Black) "5" 12) 
     b13 <- (mkButton Nothing "" 13) 
     b14 <- (mkButton Nothing "" 14) 
     b15 <- (mkButton Nothing "" 15) 
     b16 <- (mkButton (Just White) "3" 16) 
     b17 <- (mkButton Nothing "" 17) 
     b18 <- (mkButton (Just White) "5" 18) 
     b19 <- (mkButton Nothing "" 19) 
     b20 <- (mkButton Nothing "" 20) 
     b21 <- (mkButton Nothing "" 21) 
     b22 <- (mkButton Nothing "" 22) 
     b23 <- (mkButton (Just Black) "2" 23)

     -- Layout,
     pointsFirstRow <- row ([pure b0, pure b1, pure b2, pure b3, pure b4, pure b5, pure b6, pure b7, pure b8, pure b9, pure b10, pure b11])
     pointsSecondRow <- row ([pure b12, pure b13, pure b14, pure b15, pure b16, pure b17, pure b18, pure b19, pure b20, pure b21, pure b22, pure b23])
     getBody window #+ [column [pure pointsFirstRow, pure pointsSecondRow]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     --pure input # set style [("fontSize","14pt")]

     -- Interaction
     on UI.click b0 $ \ _ -> handleInput gameLogic window 0
     on UI.click b1 $ \ _ -> handleInput gameLogic window 1
     on UI.click b2 $ \ _ -> handleInput gameLogic window 2
     on UI.click b3 $ \ _ -> handleInput gameLogic window 3
     on UI.click b4 $ \ _ -> handleInput gameLogic window 4
     on UI.click b5 $ \ _ -> handleInput gameLogic window 5
     on UI.click b6 $ \ _ -> handleInput gameLogic window 6
     on UI.click b7 $ \ _ -> handleInput gameLogic window 7
     on UI.click b8 $ \ _ -> handleInput gameLogic window 8
     on UI.click b9 $ \ _ -> handleInput gameLogic window 9
     on UI.click b10 $ \ _ -> handleInput gameLogic window 10
     on UI.click b11 $ \ _ -> handleInput gameLogic window 11
     on UI.click b12 $ \ _ -> handleInput gameLogic window 12
     on UI.click b13 $ \ _ -> handleInput gameLogic window 13
     on UI.click b14 $ \ _ -> handleInput gameLogic window 14
     on UI.click b15 $ \ _ -> handleInput gameLogic window 15
     on UI.click b16 $ \ _ -> handleInput gameLogic window 16
     on UI.click b17 $ \ _ -> handleInput gameLogic window 17
     on UI.click b18 $ \ _ -> handleInput gameLogic window 18
     on UI.click b19 $ \ _ -> handleInput gameLogic window 19
     on UI.click b20 $ \ _ -> handleInput gameLogic window 20
     on UI.click b21 $ \ _ -> handleInput gameLogic window 21
     on UI.click b22 $ \ _ -> handleInput gameLogic window 22
     on UI.click b23 $ \ _ -> handleInput gameLogic window 23


firstRow btnBoard = take 12 btnBoard

secondRow btnBoard = drop 12 btnBoard

createCanvasBoard :: UI [UI Element]
createCanvasBoard =
 return ([
   (mkButton (Just White) "2" 0),(mkButton Nothing "" 1),(mkButton Nothing "" 2),(mkButton Nothing "" 3),(mkButton Nothing "" 4),(mkButton (Just Black) "5" 5),
   (mkButton Nothing "" 6),(mkButton (Just Black) "3" 7),(mkButton Nothing "" 8),(mkButton Nothing "" 9 ),(mkButton Nothing "" 10),(mkButton (Just White) "5" 11),
   (mkButton (Just Black) "5" 12),(mkButton Nothing "" 13),(mkButton Nothing "" 14),(mkButton Nothing "" 15),(mkButton (Just White) "3" 16),(mkButton Nothing "" 17),
   (mkButton (Just White) "5" 18),(mkButton Nothing "" 19),(mkButton Nothing "" 20),(mkButton Nothing "" 21),(mkButton Nothing "" 22),(mkButton (Just Black) "2" 23)
 ])
--
-- fillInitBoard :: [Canvas] -> UI [UI Element]
-- fillInitBoard cList = return [do c # UI.fillText "ss" (20,50) | c <- cList]
--

parseState :: Window -> GameState
parseState window = (GameState False (Player White Human) (Player Black Human) (Board [Point([]),
            Point([]), Point([]), Point([]), Point([]),
            Point([]),
            Point([]),
            Point([]),
            Point([]), Point([]), Point([]),
            Point([]),
            Point([]),
            Point([]), Point([]), Point([]),
            Point([]),
            Point([]),
            Point([]),
            Point([]), Point([]), Point([]), Point([]),
            Point([])] 0 0) ("Woop woop") (1,1,1,1))

handleInput :: (Move -> GameState -> GameState) -> Window -> Int -> UI Element
handleInput gl window id = do
     getBody window # set style [("backgroundColor","black"),
                                 ("textAlign","center")]
     --update (gl (parseMove (2.0,2.0)) (parseState window)) window

parseMove :: (Double, Double) -> Move
parseMove p = (Move Black (1,2))


update :: GameState -> Window -> UI Element
update (GameState True p1 p2 b s d) window = do
     getBody window # set style [("backgroundColor","black"),
                                 ("textAlign","center")]
update (GameState False p1 p2 b s d) window = do
     getBody window # set style [("backgroundColor","black"),
                                 ("textAlign","center")]

--drawCanvas :: Board -> UI
drawCanvas (Board ps _ _) c = do
     set UI.fillStyle (UI.solidColor (UI.RGB 255 255 255)) (pure c)
     UI.fillRect (0,0) canWidth canHeight c
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure c)
     UI.fillText "Woop Woop" (10,canHeight/2) c

mkPointList = mkHTML "<b>10101<b>"
