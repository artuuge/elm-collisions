import Html exposing (Html, text)
import Html.App exposing (program)
import List exposing (filterMap, foldr, length, repeat, map, map2)
import Random exposing (float, generate, list, pair)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width)
import Time exposing (Time, every, millisecond)

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Disc = { x : Float
                  , y : Float
                  , px : Float
                  , py : Float
                  , color : String
                  , radius : Float
                  , mass : Float
                  }

defaultDisc : Disc
defaultDisc = { x = 1.0
              , y = 0.5
              , px = 0.0
              , py = 0.0
              , color = "DarkRed"
--              , radius = 0.02
              , radius = 0.1
              , mass = 1.0
              }

type alias Model = { discs : List Disc }

discColors : List String
-- discColors = repeat 64 "DarkRed"
{--}
discColors = [ "DarkBlue"
             , "DarkGreen"
             , "DarkRed"
             , "DarkGoldenRod"
             , "DarkOrange"
             , "DarkCyan"
             , "DarkMagenta"
             , "DarkSlateGrey"
             ]
--}

numDiscs : Int
numDiscs = length discColors

model : Model
model = { discs = map (\color -> { defaultDisc | color = color }) discColors }

type Msg = DoNothing | Tick Time | PMomentum (List (Float, Float)) | Position (List (Float, Float))

init : (Model, Cmd Msg)
init = (model, Cmd.batch [ generate PMomentum <| list numDiscs <| pair (float 5 10) (float 0 1)
                         , generate Position <| list numDiscs <| pair (float 0.1 1.9) (float 0.1 0.9)
                         ])

showDisc : Disc -> Html Msg
showDisc dsc = circle [ cx (toString dsc.x)
                      , cy (toString dsc.y)
                      , r (toString dsc.radius)
                      , fill dsc.color
                      ] []

view : Model -> Html Msg
view mdl = svg [ viewBox "0 0 2 1"
               , width "1200px"
               , height "600px"
               , Svg.Attributes.style "background: LightGrey"
               ] <| map showDisc mdl.discs

bounce : Disc -> Disc
bounce dsc =
  let r = dsc.radius
      x = dsc.x
      y = dsc.y
      px = dsc.px
      py = dsc.py

      px' = if (x > (2.0 - r))
            then (-(abs px))
            else if (x < r)
                 then (abs px)
                 else px

      py' = if (y > (1.0 - r))
            then (-(abs py))
            else if (y < r)
                 then (abs py)
                 else py

   in { dsc | px = px', py = py' }

evolve : Disc -> Disc
evolve dsc = let m = dsc.mass
                 x = dsc.x + 0.002 * dsc.px / m
                 y = dsc.y + 0.002 * dsc.py / m
              in { dsc | x = x, y = y }

setMomentum : (Float, Float) -> Disc -> Disc
setMomentum (p, phi) dsc = { dsc | px = p * (cos (turns phi)), py = p * (sin (turns phi)) }

setPosition : (Float, Float) -> Disc -> Disc
setPosition (x, y) dsc = { dsc | x = x, y = y }

update : Msg -> Model -> (Model, Cmd Msg)
update msg mdl = (\m -> (m, Cmd.none)) <| case msg of
  DoNothing -> mdl
  Tick _ -> { mdl | discs = map (evolve << bounce) <| scatter mdl.discs }
  PMomentum ws -> { mdl | discs = map2 setMomentum ws mdl.discs }
  Position zs -> { mdl | discs = map2 setPosition zs mdl.discs }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ every (30 * millisecond) Tick ]

--

isOverlap : Disc -> Disc -> Bool
isOverlap d0 d1 = ((d0.x - d1.x)^2 + (d0.y - d1.y)^2 < (d0.radius + d1.radius)^2)

escapeTime : Disc -> Disc -> Maybe Time
escapeTime d0 d1 =
  let m0 = d0.mass
      m1 = d1.mass
      x = d0.x - d1.x
      y = d0.y - d1.y
      vx = d0.px/m0 - d1.px/m1
      vy = d0.py/m0 - d1.py/m1
      eta = ((d0.radius + d1.radius)^2 - (x^2 + y^2))/(vx^2 + vy^2)
   in if eta > 0
      then let xi = (x * vx + y * vy)/(vx^2 + vy^2)
               t = (-xi) + sqrt (xi^2 + eta)
            in Just t
      else Nothing

exchangeMomenta : (Float, Float)
               -> (Float, (Float, Float))
               -> (Float, (Float, Float))
               -> ((Float, Float), (Float, Float))
exchangeMomenta (nx, ny) (m0, (px0, py0)) (m1, (px1, py1)) =
  let nn = nx^2 + ny^2
  in if nn == 0.0
     then ((px0, py0), (px1, py1))
     else let pn0 = nx * px0 + ny * py0
              pn1 = nx * px1 + ny * py1
              (tx, ty) = (-ny, nx)
              pt0 = tx * px0 + ty * py0
              pt1 = tx * px1 + ty * py1
              q = 2.0 * (m0 * pn1 - m1 * pn0)/(m0 + m1)
              px0' = (nx * (pn0 + q) + tx * pt0)/nn
              py0' = (ny * (pn0 + q) + ty * pt0)/nn
              px1' = (nx * (pn1 - q) + tx * pt1)/nn
              py1' = (ny * (pn1 - q) + ty * pt1)/nn
           in ((px0', py0'), (px1', py1'))

extract : (a -> a -> Bool) -> List a -> Maybe a
extract f xs = case xs of
  [] -> Nothing
  x :: ys -> case extract f ys of
    Nothing -> Just x
    Just y -> case f x y of
      True -> Just x
      False -> Just y

interactEsc : (Disc -> Disc -> Maybe Time)
           -> Disc
           -> Disc
           -> (Disc, Disc)
interactEsc esct d0 d1 =
  let (nx, ny) = (d1.x - d0.x, d1.y - d0.y)
      ((px0', py0'), (px1', py1')) =
        exchangeMomenta (nx, ny) (d0.mass, (d0.px, d0.py)) (d1.mass, (d1.px, d1.py))
      opt0 = (d0, d1)
      opt1 = let d0' = { d0 | px = px0', py = py0' }
                 d1' = { d1 | px = px1', py = py1' }
              in (d0', d1')

      opts = [opt0, opt1]
      mts = map (uncurry esct) opts

      mots = map2 (\o mt -> Maybe.map (\t -> (o, t)) mt) opts mts
      ots = filterMap (\m -> m) mots

      mot = extract (\ot0 ot1 -> (snd ot0) < (snd ot1)) ots
      mo = Maybe.map fst mot
   in case mo of
        Nothing -> opt0
        Just o -> o

interact : Disc -> Disc -> (Disc, Disc)
interact = interactEsc escapeTime

fndMatch : (a -> b -> Bool) -> a -> List b -> (Maybe b, (List b, List b))
fndMatch f x ys =
  let loop ((rejected, potential) as t) =
    case potential of
      [] -> (Nothing, t)
      z :: ws -> case f x z of
        True -> (Just z, (rejected, ws))
        False -> loop (z :: rejected, ws)
  in loop ([], ys)

createCouples : (a -> a -> Bool) -> List a -> List (a, Maybe a)
createCouples f xs = case xs of
  [] -> []
  x :: zs -> let (m, (ws0, ws1)) = fndMatch f x zs
              in (x, m) :: (createCouples f (ws0 ++ ws1))

scatterGeneric : (a -> a -> Bool) -> (a -> a -> (a, a)) -> List a -> List a
scatterGeneric isoverlap interact xs =
  let qs = createCouples isoverlap xs
      f (x, m) ws = case m of
        Nothing -> x :: ws
        Just y -> let (x', y') = (uncurry interact) (x, y)
                   in x' :: y' :: ws
   in foldr f [] qs

scatter : List Disc -> List Disc
scatter = scatterGeneric isOverlap interact

