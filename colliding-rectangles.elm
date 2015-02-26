-- Tested with Elm 0.14 
-- Run "elm-make colliding-rectangles.elm --output colliding-rectangles.html" 
-- to generate html. 

-- Author: artuuge@gmail.com

-- This file illustrates how to implement a system of moving objects in space 
-- which can collide and bounce from each other. The present file contains a 
-- realization of the case where the objects are rectangles. 
-- There is a similar example: colliding-balls.elm

-- The main principle applied to handle the collisions is to minimize the 
-- time of an overlap. One needs to write a combinator which detects an 
-- overlap of a pair of objects, and then another combinator which 
-- computes the "escape time" needed to eliminate an overlap. 
-- There is a finite number of options of how to change the velocity of 
-- the objects upon a collision and the selection corresponds to the 
-- smallest escape time. 

-- One may resize the window of the browser when the program is running. 
-- The image is going to adjust itself in a natural way.  

import Color (..)
import Graphics.Collage (..) 
import Graphics.Element (..)
import Graphics.Input as I
import List as L
import Signal (..)
import Text as T 
import Time 
import Window

isNothing : Maybe a -> Bool
isNothing m = (m == Nothing)

isJust : Maybe a -> Bool
isJust = not << isNothing 

myFps : Signal Time.Time
myFps = Time.fps 300 

-- a cut-off parameter needed in the step combinator to handle a return 
-- from sleep. 
maxDelta : Float
maxDelta = 100.0

-- the datatype used to represent a rectangle
type Rct = Rct { center : (Float, Float)
               , velocity : (Float, Float)
               , geometry : (Float, Float) 
               , color : Color 
               }

rctToForm : (Int, Int) -> Rct -> Form 
rctToForm _ (Rct r) =  
  let (ax, ay) = r.geometry
      (cx, cy) = r.center
  in move (cx, cy) <| filled r.color <| rect ax ay
-- the first argument is not really used, but it is reserved 
-- as a possibility to pass the dimensions of the window. 

----------

-- (m, n) : (Int, Int) corresponds to (width, height) of the window. 
-- The size of the window can be dynamically changed by the user. 
-- fieldWidthDyn and fieldHeightDyn define the area enclosing the objects. 
widthCoeff : Float 
widthCoeff = 0.9

heightCoeff : Float
heightCoeff = 0.9

fieldWidthDyn : (Int, Int) -> Float
fieldWidthDyn (m, _) = widthCoeff * (toFloat m)

fieldHeightDyn : (Int, Int) -> Float 
fieldHeightDyn (_, n) = heightCoeff * toFloat n

halfWidthDyn : (Int, Int) -> Float 
halfWidthDyn (m, n) = (fieldWidthDyn (m, n))/ 2.0

halfHeightDyn : (Int, Int) -> Float 
halfHeightDyn (m, n) = (fieldHeightDyn (m, n))/ 2.0

leftMarginDyn : (Int, Int) -> Float
leftMarginDyn (m, n) = (-1.0) * (halfWidthDyn (m, n))

rightMarginDyn : (Int, Int) -> Float
rightMarginDyn (m, n) = halfWidthDyn (m, n)

downMarginDyn : (Int, Int) -> Float
downMarginDyn (m, n) = (-1.0) * (halfHeightDyn (m, n)) 

upMarginDyn : (Int, Int) -> Float
upMarginDyn (m, n) = halfHeightDyn (m, n)

----------

-- auxiliary combinators to work with rectangles
leftSide : Rct -> Float
leftSide (Rct rct) = 
  let (cx, _) = rct.center
      (ax, _) = rct.geometry
   in cx - ax/2.0

rightSide : Rct -> Float
rightSide (Rct rct) = 
  let (cx, _) = rct.center
      (ax, _) = rct.geometry
   in cx + ax/2.0

downSide : Rct -> Float
downSide (Rct rct) = 
  let (_, cy) = rct.center
      (_, ay) = rct.geometry
   in cy - ay/2.0

upSide : Rct -> Float
upSide (Rct rct) = 
  let (_, cy) = rct.center
      (_, ay) = rct.geometry
   in cy + ay/2.0

isLeftTo : Rct -> Rct -> Bool
isLeftTo r0 r1 = (rightSide r0 < leftSide r1) 

isRightTo : Rct -> Rct -> Bool
isRightTo r0 r1 = isLeftTo r1 r0

isDownTo : Rct -> Rct -> Bool 
isDownTo r0 r1 = (upSide r0 < downSide r1) 

isUpTo : Rct -> Rct -> Bool
isUpTo r0 r1 = isDownTo r1 r0

----------

fieldColor : Color 
fieldColor = lightGrey

view : (Int, Int) -> List Rct -> Element 
view (m, n) rcts = 
  let w = fieldWidthDyn (m, n) 
      h = fieldHeightDyn (m, n) 
   in container m n middle <| 
      collage m n <| 
        filled fieldColor (rect w h)
        :: L.map (rctToForm (m, n)) rcts

-- free motion along the geodesics
evolveRct : Time.Time -> Rct -> Rct
evolveRct dt (Rct r) = 
  let (cx, cy) = r.center 
      (vx, vy) = r.velocity 
      (cx', cy') = (cx + vx * dt, cy + vy * dt) 
  in Rct { r | center <- (cx', cy') }

-- detection of an overlap 
isOverlapRct : Rct -> Rct -> Bool
isOverlapRct r0 r1 = not ((isLeftTo r0 r1) 
                    || (isRightTo r0 r1) 
                    || (isDownTo r0 r1) 
                    || (isUpTo r0 r1)) 

-- bouce the object from the borders of the field 
-- The pair of integers (m, n) is going to stem from Window.dimensions
reflectRct : (Int, Int) -> Rct -> Rct
reflectRct (m, n) (Rct rct as r0) = 
  let lm = leftMarginDyn (m, n) 
      rm = rightMarginDyn (m, n) 
      dm = downMarginDyn (m, n) 
      um = upMarginDyn (m, n) 
      (vx, vy) = rct.velocity
      vx' = if (((leftSide r0 < lm) && vx < 0.0) || 
                ((rightSide r0 > rm) && vx > 0.0) ) then (-vx) else vx
      vy' = if (((downSide r0 < dm) && vy < 0.0) || 
                ((upSide r0 > um) && vy > 0.0) ) then (-vy) else vy
   in Rct { rct | velocity <- (vx', vy') }

-- The time necessary to remove the horizontal overlap. 
-- The return value is Nothing if the overlap cannot vanish because 
-- the  horizontal velocities are equal. 
-- If the return value is (Just t), where t is negative, then this 
-- corresponds to the moment when the overlap would had disappeared 
-- in the past if the objects were moving along their geodesics. 
escapeHoriz : Rct -> Rct -> Maybe Time.Time 
escapeHoriz (Rct r0)  (Rct r1) = 
  let (vx0, _) = r0.velocity
      (vx1, _) = r1.velocity
   in if | vx0 == vx1 -> Nothing
         | vx0 > vx1 -> 
           let t = ((rightSide (Rct r1)) - (leftSide (Rct r0)))/(vx0 - vx1) 
           in Just t
         | vx0 < vx1 -> escapeHoriz (Rct r1) (Rct r0)   

-- The time necessary to remove the vertical overlap. 
escapeVert : Rct -> Rct -> Maybe Time.Time 
escapeVert (Rct r0)  (Rct r1) = 
  let (_, vy0) = r0.velocity
      (_, vy1) = r1.velocity
   in if | vy0 == vy1 -> Nothing
         | vy0 > vy1 -> 
           let t = ((upSide (Rct r1)) - (downSide (Rct r0)))/(vy0 - vy1) 
           in Just t
         | vy0 < vy1 -> escapeVert (Rct r1) (Rct r0)   

-- The escape time definiting the moment when an overlap disappears. 
escapeTimeRct : Rct -> Rct -> Maybe Time.Time 
escapeTimeRct rct0 rct1 = 
  let mht = escapeHoriz rct0 rct1 
      mvt = escapeVert rct0 rct1 
   in case (mht, mvt) of 
        (Nothing, Nothing) -> Nothing
        (Just ht, Nothing) -> Just ht
        (Nothing, Just vt) -> Just vt
        (Just ht, Just vt) -> Just (min ht vt) 

-- Adjust the velocities of objects so that the overlap 
-- is eliminated as fast as possible. 
-- There are four options: do nothing, exchange the x-components 
-- of the velocity, exchange the y-components, or both. 
-- For each option one computes the escape time and then takes the minimal. 
-- The first argument passed corresponds to the escape time combinator. 
interactEscRct : (Rct -> Rct -> Maybe Time.Time) -> Rct -> Rct -> (Rct, Rct) 
interactEscRct esct (Rct r0) (Rct r1) = 
  let (vx0, vy0) = r0.velocity
      (vx1, vy1) = r1.velocity
      opt0 = (Rct r0, Rct r1) 
      optH = let r0' = {r0 | velocity <- (vx1, vy0) } 
                 r1' = {r1 | velocity <- (vx0, vy1) } 
              in (Rct r0', Rct r1')
      optV = let r0' = {r0 | velocity <- (vx0, vy1) } 
                 r1' = {r1 | velocity <- (vx1, vy0) } 
              in (Rct r0', Rct r1')
      optHV = let r0' = {r0 | velocity <- (vx1, vy1) } 
                  r1' = {r1 | velocity <- (vx0, vy0) } 
               in (Rct r0', Rct r1')
      opts = [opt0, optH, optV, optHV]
      escs = L.map (uncurry esct) opts
   in if (L.all isNothing escs) 
      then opt0 
      else let oes = L.map2 (,) opts escs
               oes' = oes |> L.filter (isJust << snd) 
                          |> L.map (\(opt, Just t) -> (opt, t)) 
                          |> L.sortBy snd
            in fst (L.head oes')

interactRct : Rct -> Rct -> (Rct, Rct) 
interactRct = interactEscRct escapeTimeRct

----------

-- Several generic combinators needed to group the colliding objects 
-- into pairs.

-- Move the head of ys in (xs, ys) from the second argument to the first 
-- until the condition is met. 
fndMatch : (a -> b -> Bool) -> a -> List b -> (Maybe b, (List b, List b))
fndMatch f x ys = 
  let loop ((rejected, potential) as t) = 
        case potential of 
          [] -> (Nothing, t)  
          z :: ws -> if (f x z) 
                     then (Just z, (rejected, ws)) 
                     else loop (z :: rejected, ws)         
   in loop ([], ys)

-- Find a match for each element of the list taking the first 
-- available choice.  
createCouples : (a -> a -> Bool) -> List a -> List (a, Maybe a) 
createCouples f xs = 
  case xs of 
    [] -> []
    x :: zs -> let (m, (ws0, ws1)) = fndMatch f x zs
               in (x, m) :: (createCouples f (ws0 ++ ws1)) -- reverse ws0 

-- Split the collection of objects into matching pairs and  
-- update their states. 
scatter : (a -> a -> Bool) -> (a -> a -> (a, a)) -> List a -> List a
scatter isoverlap interact xs = 
  let qs = createCouples isoverlap xs 
      f (x, m) ws = 
        case m of 
          Nothing -> x :: ws 
          Just y -> let (x', y') = (uncurry interact) (x, y) 
                     in x' :: y' :: ws  
   in L.foldr f [] qs

----------

-- create a list of overlapping pairs of objects and 
-- adjust the veocities to minimize the time of overlap
scatterRct : List Rct -> List Rct 
scatterRct = scatter isOverlapRct interactRct

-- define a type to unite the signals from Time and Window.dimensions
type TW = TW Time.Time (Int, Int)

stepRaw : TW -> List Rct -> List Rct 
stepRaw (TW dt (m, n)) rcts = 
  L.map ((evolveRct dt) << (reflectRct (m, n))) (scatterRct rcts)

-- if the computer returns from a sleeping mode the dt can be too large 
-- maxDelta defines a cut-off to avoid a huge dt
step : TW -> List Rct -> List Rct 
step (TW dt (m, n) as tw) rcts = 
  if (dt > maxDelta) 
  then rcts 
  else stepRaw tw rcts

----------

mySignal : Signal TW
mySignal = sampleOn myFps (TW <~ myFps ~ Window.dimensions)

main : Signal Element
main = view <~ Window.dimensions ~ foldp step initialConfig mySignal

----------

-- The test configuration.

initializeRct : (Float, Float) 
             -> (Float, Float) 
             -> (Float, Float) 
             -> Color 
             -> Rct
initializeRct (cx, cy) (vx, vy) (w, h) c = 
  Rct { center = (cx, cy)
      , velocity = (vx, vy)
      , geometry = (w, h)
      , color = c 
      }

defaultGeometry : (Float, Float)
-- defaultGeometry = (60.0, 40.0)
defaultGeometry = (120.0, 80.0)
-- defaultGeometry = (240.0, 160.0)

initRct : (Float, Float) -> (Float, Float) -> Color -> Rct
initRct (cx, cy) (vx, vy) c = 
  initializeRct (cx, cy) (vx, vy) defaultGeometry c

initialConfig : List Rct 
initialConfig = [ initRct (400.0, 100.0) (-0.1, 0.2) darkBlue
                , initRct (-150.0, 70.0) (0.4, 0.3) darkGreen
                , initRct (300.0, -200.0) (0.6, -0.5) darkRed
                , initRct (-100.0, -200.0) (0.7, -0.8) darkYellow
                , initRct (-450.0, 0.0) (-0.35, 0.25) darkOrange
                , initRct (-420.0, -300.0) (0.3, 0.7) lightBlue
                , initRct (150.0, 200.0) (-0.3, 0.45) darkPurple
                , initRct (-420.0, 280.0) (-0.7, -0.15) lightBrown
                ]

