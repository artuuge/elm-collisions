-- Tested with Elm 0.14
-- Run "elm-make hello-elm.elm --output hello-elm.html" to generate html. 

-- Author: artuuge@gmail.com

-- filename: hello-elm.elm
-- This is a simple demo showing a rotating and periodically scaling in size 
-- "Hello, Elm!" text message. 

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Signal (..)
import Text as T
import Time (..) 
import Window

main : Signal Element
main = scene <~ Window.dimensions ~ (every (0.01*second))

-- amplitude
amp : Float
amp = 100.0

-- angular velocity
omega : Float 
omega = 0.003

-- w is width, h is height, and t is time
scene : (Int,Int) -> Float -> Element
scene (w,h) t = collage w h [ 
  "Hello, Elm!" 
    |> T.fromString 
    |> T.color darkRed
    |> T.centered 
    |> toForm 
    |> move (amp * (cos (omega * t)), amp * (sin (omega * t))) 
    |> rotate (degrees (0.05*t)) 
    |> scale (2 + sin (0.005*t)) 
  ]
