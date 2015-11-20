-- Filename: checkbox.elm

-- Language: Elm 0.15
-- You can run this code at http://elm-lang.org/try

-- Author: artuuge@gmail.com

-- This file illustrates how to create a single checkbox which changes its 
-- color depending on whether it is selected or not, and whether one 
-- hovers over it with a mouse pointer or not. 

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input as I 
import Maybe as M
import Signal exposing (..)
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

view : Mailbox (Maybe Action) -> State -> Element 
view chan st = 
  display st 
    |> I.hoverable (message chan.address << Just << Hover)
    |> I.clickable (message chan.address << Just <| Click) 

step : Action -> State -> State 
step ac st = case ac of 
  Click   -> { st | selected <- (not st.selected) }
  Hover b -> { st | excited <- b }

-- There is no particular reason to use the Maybe functor, but 
-- it is convenient to initialize the channel to Nothing. 
-- Creating a channel is an IMPURE part of Elm. 
actions : Mailbox (Maybe Action)
actions = mailbox Nothing

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
    <~ foldp (maybe identity step) initialState actions.signal
