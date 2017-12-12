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
canWidth  = 800
canHeight = 700

setup (Interface hm) firstState = startGUI defaultConfig (baseHTML firstState hm)

baseHTML firstState gameLogic window  = do
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     dialog  <- mkHTML "<i>Action</i>)="      -- The text "Action"
     p1      <- mkHTML "<b>White<b>"          -- Shows which player's turn it is
     p2      <- mkHTML "<b>Black<b>"          -- Shows the other player
     invis   <- mkPointList                   -- Only used in parsing state (never shown)
     
     -- Layout
     formula <- row [pure canvas, pure dialog]
     getBody window #+ [column [pure formula]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     --pure input # set style [("fontSize","14pt")]

     -- Interaction
     on UI.click     canvas  $ \ _ -> handleInput gameLogic window

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

handleInput :: (Move -> GameState -> GameState) -> Window -> UI Element
handleInput gl window = do
     update (gl (parseMove (2.0,2.0)) (parseState window)) window

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
