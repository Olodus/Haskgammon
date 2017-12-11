import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Haskgammon
import UserInterface

canWidth,canHeight :: Num a => a
canWidth  = 800
canHeight = 700

main = startGUI defaultConfig setup

setup window =
  do -- Elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     
     buttonList <- createBoard stdStart
     
     -- Layout
     formula <- row (firstHalf buttonList)
     formula2 <- row (secondHalf buttonList)
     getBody window #+ [column [pure formula,pure formula2]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction
     --on UI.click     button1  $ \ _ -> readAndDraw buttonList
     --on valueChange' input $ \ _ -> readAndDraw buttonList

createBoard :: Board ->  [UI (Element)]
createBoard (Board [] _ _) = []
createBoard (Board (x:xs) wh bl) = [(mkButton (show (length (checkers x))))] ++ createBoard (Board xs wh bl)

firstHalf bList = take 12 bList

secondHalf bList = drop 12 bList

--readAndDraw bl =
--  do 
     -- The following code draws the formula text in the canvas.
     -- It should be replaced with code that draws the graph of the function.
--     set UI.fillStyle (UI.solidColor (UI.RGB 255 255 255)) (pure canvas)
--     UI.fillRect (0,0) canWidth canHeight canvas
--     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
--    UI.fillText s (10,canHeight/2) canvas
