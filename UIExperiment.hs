module UIExperiment where

import Data.Char
import System.Random
import Debug.Trace
import Control.Monad.State.Lazy as MS
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import ThreepennyPages

import BackgammonTypes

data Interface = Interface
  { iHandleMove :: Move -> GameState -> GameState }

canWidth,canHeight :: Num a => a
canWidth  = 800
canHeight = 700

start = setup (GameState True (Player White Human) (Player Black Human) (createEmptyBoard) ("bla") (1,1,1,1))


handleMove :: Move -> GameState -> GameState
handleMove m g = (GameState True (Player White Human) (Player Black Human) (createEmptyBoard) ("Woop woop") (1,1,1,1))

setup firstState = startGUI defaultConfig (baseHTML firstState)

baseHTML firstState window  = do
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     dialog  <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="

     --state <- get
     
     -- Layout
     formula <- row [pure canvas, pure dialog]
     getBody window #+ [column [pure formula]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     --pure input # set style [("fontSize","14pt")]

     -- Interaction
     on UI.click     canvas  $ \ _ -> handleInput window --(runState updateState $ firstState)

parseState :: Window -> GameState
parseState window = (GameState True (Player White Human) (Player Black Human) (createEmptyBoard) ("Woop woop") (1,1,1,1))

handleInput :: Window -> UI Element
handleInput window = do
     update (handleMove (parseMove (2.0,2.0)) (parseState window)) window

parseMove :: (Double, Double) -> Move
parseMove p = (Move Black (1,2))

--updateState :: State Move GameState
--updateState = do
--     currentState <- MS.get
--     put (handleMove (m) (currentState))
--     newState <- MS.get
--     return newState

update :: GameState -> Window -> UI Element
update (GameState end p1 p2 b s d) window = do
     getBody window # set style [("backgroundColor","black"),
                                 ("textAlign","center")]
--     set UI.fillStyle (UI.solidColor (UI.RGB 255 255 255)) (pure canvas)
--     UI.fillRect (0,0) canWidth canHeight canvas
--     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
--     UI.fillText s (10,canHeight/2) canvas
