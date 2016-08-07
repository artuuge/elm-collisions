import Array exposing (repeat, toIndexedList)
import Html exposing (div, Html, text)
import Html.App exposing (program)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseOver, onMouseOut)
import List exposing (filter, length, map)
import Mouse exposing (clicks)

main = program { init = init
               , view = view
               , update = update
               , subscriptions = subscriptions
               }

type alias Box = {
    excited : Bool
  , selected : Bool
  , locked : Bool
  }

defaultBox : Box
defaultBox = {
    excited = False
  , selected = False
  , locked = False
  }

type alias Model = {
    iboxes : List (Int, Box)
  }

numBoxes : Int
numBoxes = 8

model : Model
model = { iboxes = toIndexedList (repeat numBoxes defaultBox) }

type Msg = DoNothing | MouseOver Int | MouseOut Int | MouseClick | Unlock | Lock

init : (Model, Cmd Msg)
init = (model, Cmd.batch [])

viewIBox : (Int, Box) -> Html Msg
viewIBox (i, box) = div [ style [ ("width", "80px")
                                , ("height", "60px")
                                , ("background-color", case (box.excited, box.selected) of
                                    (False, False) -> "MediumPurple"
                                    (True, False) -> "LightBlue"
                                    (False, True) -> "DarkRed"
                                    (True, True) -> "DarkOrange"
                                  )
                                , ("display", "inline-block")
                                ]
                        , onMouseOver (MouseOver i)
                        , onMouseOut (MouseOut i)
                        ] []

whichSelected : Model -> List Int
whichSelected mdl =
  map fst <| filter (\ib -> (snd ib).selected) mdl.iboxes

view : Model -> Html Msg
view mdl = div [ style [("float", "left")] ] [ div [ onMouseOver Unlock
                                                   , onMouseOut Lock
                                                   , style [ ("width", (toString (numBoxes * 80)) ++ "px")
                                                           , ("", (toString (numBoxes * 60)) ++ "px")
                                                           ]
                                                   ] [ div [] (map viewIBox mdl.iboxes) ]
                                             , div [] [ text ("Current selection: " ++ (toString (whichSelected mdl))) ]
                                             ]

updateIBox : Msg -> (Int, Box) -> (Int, Box)
updateIBox msg (i, box) = (\b -> (i, b)) <| case msg of
  DoNothing -> box
  MouseOver j -> case i == j of
    True -> { box | excited = True }
    False -> box
  MouseOut j -> case i == j of
    True -> { box | excited = False }
    False -> box
  MouseClick -> case box.locked of
    True -> box
    False -> { box | selected = box.excited }
  Unlock -> { box | locked = False }
  Lock -> { box | locked = True }

update : Msg -> Model -> (Model, Cmd Msg)
update msg mdl = (\m -> (m, Cmd.batch [])) <|
  { mdl | iboxes = map (updateIBox msg) mdl.iboxes }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ clicks (always MouseClick) ]

