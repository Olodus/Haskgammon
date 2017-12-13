-- | This module defines helper functions for creating web pages using
-- "Graphics.UI.Threepenny". (Similar to the Pages module for Haste.)
module ThreepennyPages(
  -- * Building web pages
  Element,UI.Canvas,mkCanvas,mkInput,mkButton,mkSlider,
  mkHTML,
  -- * Events
  valueChange',mousedown'
  ) where

import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import qualified Data.Aeson              as JSON
import BackgammonTypes

-- | @mkInput width init@ makes an input element with the specified width and
-- initial text
mkInput :: Int -> String -> UI Element
mkInput width init = do
    UI.input # set UI.type_ "text"
             # set UI.size (show width)
             # set value init

-- | @mkButton label@ makes a clickable button with the given label
mkButton :: Maybe Color -> String -> Int -> UI Element
mkButton (Just White) label id = UI.button
                    # set text label
                    # set style [("width", "50px"),
                                 ("height", "100px"),
                                 ("backgroundColor", "white")
                                  ]
                    # set (attr "id") (show id)
mkButton (Just Black) label id = UI.button
                    # set text label
                    # set style [("width", "50px"),
                                 ("height", "100px"),
                                 ("backgroundColor", "black"),
                                 ("color", "white")
                                  ]
                    # set (attr "id") (show id)

mkButton Nothing label id = UI.button
                    # set text label
                    # set style [("width", "50px"),
                                 ("height", "100px"),
                                 ("backgroundColor", "lightgray")]
                    # set (attr "id") (show id)


-- | @mkSlider (min,max) init@ create a slider
mkSlider :: (Int,Int) -> Int -> UI Element
mkSlider (min,max) init = UI.input # set UI.type_ "range"
                                   # set (attr "min") (show min)
                                   # set (attr "max") (show max)
                                   # set value (show init)

-- | @mkHTML html@ makes an element with the specified HTML content
mkHTML :: String -> UI Element
mkHTML html = UI.span # set UI.html html


-- | @mkCanvas width height@ makes a "Canvas" of the specified dimensions
mkCanvas :: Int -> Int -> UI UI.Canvas
mkCanvas width height =
    UI.canvas # set style [("border","1px solid black"),
                           ("backgroundColor","white")]
              # set UI.width width
              # set UI.height height


--------------------------------------------------------------------------------

--onChange el f = onEvent (domEvent "change" el) $ \ _ ->
--                f =<< el # get UI.value

-- | Event that occurs when the /user/ has changed the value of the
-- input element.
valueChange' :: Element -> Event String
valueChange' el = unsafeMapUI el (const $ get value el) (domEvent "change" el)

unsafeMapUI el f = unsafeMapIO (\a -> getWindow el >>= \w -> runUI w (f a))

----------------

-- * Workaround for buggy mousedown :: Element -> Event (Int,Int)

-- | Mouse down event.
-- The mouse coordinates are relative to the element, with (0,0) in the
-- top left corner.
mousedown' :: Element -> Event (Double,Double)
mousedown' = fmap readCoordinates . domEvent "mousedown"


readCoordinates :: EventData -> (Double,Double)
readCoordinates json =
   case JSON.fromJSON json of
     JSON.Success [x,y] -> (x,y)
     _ -> error (show json) -- Array [Number 316.0,Number 249.125]
