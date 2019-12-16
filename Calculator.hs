-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe
import Expr

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zoomIn  <- mkButton "ZoomIn"             -- The zoom in button
     zoomOut <- mkButton "ZoomOut"            -- The zoom out button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input 0.04 canvas
     on UI.click     zoomIn  $ \ _ -> do 
                let scale = 0.04*2
                readAndDraw input scale canvas
     on valueChange' input $ \ _ -> readAndDraw input 0.04 canvas


readAndDraw :: Element -> Double -> Canvas -> UI ()
readAndDraw input scale canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     let expr = fromJust $ readExpression formula 
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points expr scale (canWidth,canHeight)) canvas
     where readExpression formula = case readExpr formula of 
                    Just exp -> Just exp
                    Nothing -> Nothing

-- H --

points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = zip mathToUIX mathToUIY
  where y = (map (eval exp) x)
        x = map (*scale) [-w'..w']
        w' = fromIntegral $ canWidth `div` 2
        h' = fromIntegral $ canHeight `div` 2
        mathToUIX = map (\i -> w' + i/scale) x
        mathToUIY = map (\i -> h' - i/scale) y
    
