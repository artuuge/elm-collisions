-- Tested with Elm 0.14
-- Run "elm-make checkbox-single.elm --output checkbox-single.html" 
-- to generate html. 

-- Author: artuuge@gmail.com

-- filename: checkbox-single.elm
-- This file illustrates how to create a single checkbox which changes its 
-- color depending on whether it is selected or not, and whether one 
-- hovers over it with a mouse pointer or not. 

import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Graphics.Input as I 
import Maybe as M
import Signal (..)
import Text as T

type alias State = { width : Int
                   , height : Int
                   , selected : Bool
                   , excited : Bool 
                   }

type Action = Click | Hover Bool 

initialState : State
initialState = { width = 80
               , height = 60
               , selected = False
               , excited = False 
               }

display : State -> Element 
display st = 
  let (w, h) = (st.width, st.height) 
      (s, e) = (st.selected, st.excited) 
      c = case (s, e) of 
        (False, False) -> lightPurple
        (False, True)  -> lightBlue
        (True, False)  -> darkRed
        (True, True)   -> darkOrange
  in collage w h [ rect (toFloat w) (toFloat h) |> filled c ]

view : Channel (Maybe Action) -> State -> Element 
view chan st = 
  display st 
    |> I.hoverable (send chan << Just << Hover)
    |> I.clickable (send chan << Just <| Click) 

step : Action -> State -> State 
step ac st = case ac of 
  Click   -> { st | selected <- (not st.selected) }
  Hover b -> { st | excited <- b }

-- There is no particular reason to use the Maybe functor, but 
-- it is convenient to initialize the channel to Nothing. 
-- Creating a channel is an IMPURE part of Elm. 
actions : Channel (Maybe Action)
actions = channel Nothing

maybe : b -> (a -> b) -> Maybe a -> b
maybe y f mx = case mx of 
  Nothing -> y
  Just x  -> f x

-- If the channel contains Nothing, then the state does not change. 
-- This is implemented with (maybe identity step) which has the type 
-- (Maybe Action -> State -> State). In the definition of maybe, 
-- put: a = Action, b = (State -> State). 
main : Signal Element 
main = view actions 
    <~ foldp (maybe identity step) initialState (subscribe actions)

