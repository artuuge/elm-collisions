import Html exposing (Html)
import Html.App exposing (program)
import List exposing (filterMap, foldr, length, repeat, map, map2)
import Random exposing (float, generate, list, pair)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (fill, height, r, viewBox, width, x, y)
import Time exposing (Time, every, millisecond)

main =
  program { init = init, view = view, update = update, subscriptions = subscriptions }

type alias Brick = { x : Float
                   , y : Float
                   , px : Float
                   , py : Float
                   , color : String
                   , width : Float
                   , height : Float
                   , mass : Float
                   }

defaultBrick : Brick
defaultBrick = { x = 1.0
               , y = 0.5
               , px = 0.0
               , py = 0.0
               , color = "DarkRed"
               , width = 0.08
               , height = 0.04
               -- , width = 0.2
               -- , height = 0.1
               , mass = 1.0
               }

type alias Model = { bricks : List Brick }

brickColors : List String
brickColors = repeat 64 "DarkRed"
{--
brickColors = [ "DarkBlue"
              , "DarkGreen"
              , "DarkRed"
              , "DarkGoldenRod"
              , "DarkOrange"
              , "DarkCyan"
              , "DarkMagenta"
              , "DarkSlateGrey"
              ]
--}

numBricks : Int
numBricks = length brickColors

model : Model
model = { bricks = map (\color -> { defaultBrick | color = color }) brickColors }

type Msg = DoNothing | Tick Time | PMomentum (List (Float, Float)) | Position (List (Float, Float))

init : (Model, Cmd Msg)
init = (model, Cmd.batch [ generate PMomentum <| list numBricks <| pair (float 1 10) (float 0 1)
                         , generate Position <| list numBricks <| pair (float 0.0 (2.0 - defaultBrick.width))
                                                                       (float 0.0 (1.0 - defaultBrick.height))
                         ])

showBrick : Brick -> Html Msg
showBrick brck = rect [ x (toString brck.x)
                      , y (toString brck.y)
                      , width (toString brck.width)
                      , height (toString brck.height)
                      , fill brck.color
                      ] []

view : Model -> Html Msg
view mdl = svg [ viewBox "0 0 2 1"
               , width "1200px"
               , height "600px"
               , Svg.Attributes.style "background: LightGrey"
               ] <| map showBrick mdl.bricks

bounce : Brick -> Brick
bounce brck = let w = brck.width
                  h = brck.height
                  x = brck.x
                  y = brck.y
                  px = brck.px
                  py = brck.py

                  px' = if (x > (2.0 - w))
                        then (-(abs px))
                        else if (x < 0.0)
                             then (abs px)
                             else px

                  py' = if (y > (1.0 - h))
                        then (-(abs py))
                        else if (y < 0.0)
                             then (abs py)
                             else py

               in { brck | px = px', py = py' }

evolve : Brick -> Brick
evolve brck = let m = brck.mass
                  x = brck.x + 0.002 * brck.px / m
                  y = brck.y + 0.002 * brck.py / m
               in { brck | x = x, y = y }

setMomentum : (Float, Float) -> Brick -> Brick
setMomentum (p, phi) brck = { brck | px = p * (cos (turns phi)), py = p * (sin (turns phi)) }

setPosition : (Float, Float) -> Brick -> Brick
setPosition (x, y) brck = { brck | x = x, y = y }

{--
scatter : List Brick -> List Brick
scatter bs = bs
--}

update : Msg -> Model -> (Model, Cmd Msg)
update msg mdl = (\m -> (m, Cmd.none)) <| case msg of
  DoNothing -> mdl
  Tick _ -> { mdl | bricks = map (evolve << bounce) <| scatter mdl.bricks }
  PMomentum ws -> { mdl | bricks = map2 setMomentum ws mdl.bricks }
  Position zs -> { mdl | bricks = map2 setPosition zs mdl.bricks }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ every (30 * millisecond) Tick ]

--

isOverlap : Brick -> Brick -> Bool
isOverlap b0 b1 = not ( (b0.x > b1.x + b1.width)
                     || (b0.x + b0.width < b1.x)
                     || (b0.y > b1.y + b1.height)
                     || (b0.y + b0.height < b1.y)
                      )

escapeHoriz : Brick -> Brick -> Maybe Time
escapeHoriz b0 b1 =
  let vx0 = b0.px / b0.mass
      vx1 = b1.px / b1.mass
   in case compare vx0 vx1 of
        EQ -> Nothing
        GT -> Just <| ((b1.x + b1.width) - b0.x)/(vx0 - vx1)
        LT -> Just <| ((b0.x + b0.width) - b1.x)/(vx1 - vx0)

escapeVert : Brick -> Brick -> Maybe Time
escapeVert b0 b1 =
  let vy0 = b0.py / b0.mass
      vy1 = b1.py / b1.mass
   in case compare vy0 vy1 of
        EQ -> Nothing
        GT -> Just <| ((b1.y + b1.height) - b0.y)/(vy0 - vy1)
        LT -> Just <| ((b0.y + b0.height) - b1.y)/(vy1 - vy0)

escapeTime : Brick -> Brick -> Maybe Time
escapeTime b0 b1 =
  let mth = escapeHoriz b0 b1
      mtv = escapeVert b0 b1
   in case (mth, mtv) of
        (Nothing, Nothing) -> Nothing
        (Just th, Nothing) -> Just th
        (Nothing, Just tv) -> Just tv
        (Just th, Just tv) -> Just (min th tv)

extract : (a -> a -> Bool) -> List a -> Maybe a
extract f xs = case xs of
  [] -> Nothing
  x :: ys -> case extract f ys of
    Nothing -> Just x
    Just y -> case f x y of
      True -> Just x
      False -> Just y


exchangeMomentaX : (Brick, Brick) -> (Brick, Brick)
exchangeMomentaX (b0, b1) =
  let b0' = { b0 | px = b1.px }
      b1' = { b1 | px = b0.px }
   in (b0', b1')

exchangeMomentaY : (Brick, Brick) -> (Brick, Brick)
exchangeMomentaY (b0, b1) =
  let b0' = { b0 | py = b1.py }
      b1' = { b1 | py = b0.py }
   in (b0', b1')

interactEsc : (Brick -> Brick -> Maybe Time)
           -> Brick
           -> Brick
           -> (Brick, Brick)
interactEsc esct b0 b1 =
  let opt0 = (b0, b1)
      opt1 = exchangeMomentaX (b0, b1)
      opt2 = exchangeMomentaY (b0, b1)
      opt3 = exchangeMomentaY (exchangeMomentaX (b0, b1))

      opts = [opt0, opt1, opt2, opt3]

      mts = map (uncurry esct) opts

      mots = map2 (\o mt -> Maybe.map (\t -> (o, t)) mt) opts mts
      ots = filterMap (\m -> m) mots

      mot = extract (\ot0 ot1 -> (snd ot0) < (snd ot1)) ots
      mo = Maybe.map fst mot
   in case mo of
        Nothing -> opt0
        Just o -> o

interact : Brick -> Brick -> (Brick, Brick)
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

scatter : List Brick -> List Brick
scatter = scatterGeneric isOverlap interact

