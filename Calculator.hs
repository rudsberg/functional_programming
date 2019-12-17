-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Data.Maybe
import Expr
import Parsing

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

scale :: Double
scale = 0.04

main :: IO ()
main = startGUI defaultConfig setup


setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     header  <- mkHTML "Welcome to the Graph Calculator"
     canvas  <- mkCanvas canWidth canHeight     -- The drawing area
     fx      <- mkHTML "<b>f</b>(<b>x</b>)= "   -- The text "f(x)="
     input   <- mkInput 22 "x"                  -- The formula input
     draw    <- mkButton "Draw graph"           -- The draw button
     differ  <- mkButton "Differentiate"        -- The differentiate button
     ztxt    <- mkHTML "<b>Zoom:</b>"           -- The text "zoom:"
     zoom    <- mkSlider (1,4) 1                -- The zoom slider
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     slider <- row [pure ztxt, pure zoom]
     getBody window #+ [column [row [pure header], row [pure canvas], row [pure formula], row [pure slider], row [pure draw, pure differ]]] 

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"), ("textAlign","center")]
     askWindow # set title "Graph Calculator" 
     pure fx # set style [("fontSize","16pt")]
     pure draw # set style [("fontSize","16pt"),("backgroundColor","white"),("borderColor","black")]
     pure differ # set style [("fontSize","16pt"),("backgroundColor","white"),("borderColor","black")]
     pure input # set style [("fontSize","16pt")]
     pure header # set style  [("fontSize","20pt")]
     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input scale canvas
     
     on valueChange' zoom  $ \ _ -> do 
           zoomVal <- get value zoom
           let z = read zoomVal
           let scale' = scale/z
           readAndDraw input scale' canvas
           
           {-
     on UI.click     zoomOut  $ \ _ -> do 
           let scale' = scale*2
           readAndDraw input scale' canvas
           -}
     on UI.click     differ $ \ _ -> do
              formula <- get value input 
              pure input # set value (diff formula)
              readAndDraw input scale canvas
            
     on valueChange' input $ \ _ -> readAndDraw input scale canvas


     where diff :: String -> String
           diff formula = showExpr $ differentiate $ fromJust $ readExpr formula
          



readAndDraw :: Element -> Double -> Canvas -> UI ()
readAndDraw input scale canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     let expr = fromJust $ readExpr formula 
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     --askWindow # set title "Graph Calculator" 
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points expr scale (canWidth,canHeight)) canvas


-- H --

points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width,height) = zip mathToUIX mathToUIY
  where y = (map (eval exp) x)
        x = map (*scale) [-w'..w']
        w' = fromIntegral $ canWidth `div` 2
        h' = fromIntegral $ canHeight `div` 2
        mathToUIX = map (\i -> w' + i/scale) x
        mathToUIY = map (\i -> h' - i/scale) y

    
