-- Filename: hello-elm.elm

-- Language: Elm 0.16
-- You can run this code at http://elm-lang.org/try

-- Author: artuuge@gmail.com

-- This is a simple demo showing a rotating and periodically scaling in size 
-- "Hello, Elm!" text message. 

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing (..)
import Text as T
import Time exposing (..) 
import Window

main : Signal Element
main = map2 scene Window.dimensions (every (0.01*second))

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
    |> centered
    |> toForm 
    |> move (amp * (cos (omega * t)), amp * (sin (omega * t))) 
    |> rotate (degrees (0.05*t)) 
    |> scale (2 + sin (0.005*t)) 
  ]
