import Html exposing (Html, div, text)
import Html.App exposing (program)
import Html.Attributes exposing (style)
import Task exposing (perform)
import Time exposing (Time, every, inMinutes, millisecond, now)
import List exposing (intersperse)
import String exposing (concat)

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Model = { mt : Maybe Time }

model : Model
model = { mt = Nothing }

type Msg = NoTime | Tick Time

init : (Model, Cmd Msg)
init = (model, Cmd.batch [ perform (always NoTime) Tick now ])

view : Model -> Html Msg
view mdl = case mdl.mt of
  Nothing -> div [] []
  Just t -> let wf = 800
                hf = 450
                w = wf * 0.3
                h = hf * 0.15
                r = turns <| (\u -> u - (toFloat (floor u))) <| inMinutes t
                dx = wf * (0.45 + 0.1 * (cos (-19 * r)))
                dy = hf * (0.45 + 0.1 * (sin (-23 * r)))
                sx = 2 + 1.0 * cos (7 * r)
                sy = sx
                phi = 11 * r
             in div [ style [ ("color", "DarkRed")
                            , ("background-color", "DarkKhaki")
                            , ("width", (toString w) ++ "px")
                            , ("height", (toString h) ++ "px")
                            , ("tansform-origin", concat <| intersperse " " <| [
                                                      (toString (0.5 * w)) ++ "px"
                                                    , (toString (0.5 * h)) ++ "px"
                                                    ])
                            , ("transform", concat <| intersperse " " <| [
                                                "translate(" ++ (toString dx) ++ "px, " ++ (toString dy) ++ "px)"
                                              , "scale(" ++ (toString sx) ++ ", " ++ (toString sy) ++ ")"
                                              , "rotate(" ++ (toString phi) ++ "rad)"
                                              ])
                            , ("text-align", "center")
                            , ("display", "table-cell")
                            , ("vertical-align", "middle")
                            ]
                    ] [ text ("Hello, Elm!") ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg mdl = (\m -> (m, Cmd.none)) <| case msg of
  NoTime -> mdl
  Tick t -> { mdl | mt = Just t }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ every millisecond Tick ]

