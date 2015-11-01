-- Filename: collisions.elm

-- Language: Elm 0.15
-- You can run this code at http://elm-lang.org/try

-- Author: artuuge@gmail.com

-- This file illustrates how to implement a system of moving objects in space 
-- which can collide and bounce from each other. The present file contains a 
-- realization of the case where the objects are two dimensional balls. 
-- There is a similar example: rectangles.elm

-- The main principle applied to handle the collisions is to minimize the 
-- time of an overlap. One needs to write a combinator which detects an 
-- overlap of a pair of objects, and then another combinator which 
-- computes the "escape time" needed to eliminate an overlap. 
-- There is a finite number of options of how to change the velocity of 
-- the objects upon a collision and the selection corresponds to the 
-- smallest escape time. 

-- One may resize the window of the browser when the program is running. 
-- The image is going to adjust itself in a natural way.  

import Color exposing (..)
import Graphics.Collage exposing (..) 
import Graphics.Element exposing (..)
import Graphics.Input as I
import List as L
import Signal exposing (..)
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

-- the datatype representing a circle (2d ball)
type Circle = Circle { center : (Float, Float)
                     , momentum : (Float, Float)
                     , mass : Float
                     , radius : Float
                     , color : Color 
                     }

circleToForm : (Int, Int) -> Circle -> Form 
circleToForm _ (Circle circ) =  
  let r = circ.radius
      (cx, cy) = circ.center
  in move (cx, cy) <| filled circ.color <| circle r
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

fieldColor : Color
fieldColor = lightGrey

view : (Int, Int) -> List Circle -> Element 
view (m, n) circles = 
  let w = fieldWidthDyn (m, n) 
      h = fieldHeightDyn (m, n) 
   in container m n middle <| 
      collage m n <| 
        filled fieldColor (rect w h)
        :: L.map (circleToForm (m, n)) circles

getVelocity : Circle -> (Float, Float) 
getVelocity (Circle circ) = 
  let (px, py) = circ.momentum
      m0 = circ.mass
   in (px/ m0, py/ m0)

-- free motion along the geodesics
evolveCircle : Time.Time -> Circle -> Circle
evolveCircle dt (Circle circ as c) = 
  let (cx, cy) = circ.center
      (vx, vy) =  getVelocity c
      (cx', cy') = (cx + vx * dt, cy + vy * dt) 
  in Circle { circ | center <- (cx', cy') }

-- detection of an overlap
isOverlapCircle : Circle -> Circle -> Bool 
isOverlapCircle (Circle circ0) (Circle circ1) = 
  let (cx0, cy0) = circ0.center 
      r0 = circ0.radius
      (cx1, cy1) = circ1.center 
      r1 = circ1.radius
   in (cx1 - cx0)^2 + (cy1 - cy0)^2 < (r0 + r1)^2

-- bouce the object from the borders of the field 
-- The pair of integers (m, n) is going to stem from Window.dimensions
reflectCircle : (Int, Int) -> Circle -> Circle
reflectCircle (m, n) (Circle circ) = 
  let lm = leftMarginDyn (m, n)
      rm = rightMarginDyn (m, n)
      dm = downMarginDyn (m, n)
      um = upMarginDyn (m, n)
      (px, py) = circ.momentum 
      (cx, cy) = circ.center
      r = circ.radius
      px' = if ((cx - r < lm && px < 0) || (cx + r > rm && px > 0)) 
            then (-px) 
            else px
      py' = if ((cy - r < dm && py < 0) || (cy + r > um && py > 0)) 
            then (-py) 
            else py
   in Circle {circ | momentum <- (px', py') }

-- The time necessary to remove an overlap if the objects stay on the geodesics. 
-- The return value Nothing corresponds to the fact that the overlap state 
-- cannot change. 
escapeTimeCircle : Circle -> Circle -> Maybe Time.Time 
escapeTimeCircle (Circle circ0) (Circle circ1) = 
  let (cx0, cy0) = circ0.center 
      (px0, py0) = circ0.momentum
      m0 = circ0.mass 
      r0 = circ0.radius
      (cx1, cy1) = circ1.center 
      (px1, py1) = circ1.momentum
      m1 = circ1.mass 
      cx = cx0 - cx1
      cy = cy0 - cy1
      vx = px0/m0 - px1/m1
      vy = py0/m0 - py1/m1
      r1 = circ1.radius 
      eta = ((r0 + r1)^2 - (cx^2 + cy^2))/(vx^2 + vy^2) 
   in if eta > 0.0 
      then let xi = (cx * vx + cy * vy)/(vx^2 + vy^2) 
               t = (-xi) + sqrt (xi^2 + eta) 
            in Just t
      else Nothing 

-- Compute the momenta of the balls after the collision. 
-- The first argument defines the direction along which there is an 
-- exchange of momenta. If the return value is (Just t) with a negative t, 
-- this corresponds to the moment in the past when an overlap had disappeared if 
-- the balls evolved along the geodesics. 
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

maybe : b -> (a -> b) -> Maybe a -> b
maybe y f mx = 
  case mx of 
    Nothing -> y
    Just x -> f x

-- Adjust the velocities of objects so that the overlap 
-- is eliminated as fast as possible. 
-- There are two options: to do nothing, or to exchange the momenta 
-- along the line connecting the centers if the circles. 
-- The selection corresponds to the minimal escape time passed as the 
-- first argument. 
interactEscCircle : (Circle -> Circle -> Maybe Time.Time) 
                 -> Circle 
                 -> Circle 
                 -> (Circle, Circle) 
interactEscCircle esct (Circle circ0) (Circle circ1) = 
  let (cx0, cy0) = circ0.center 
      (px0, py0) = circ0.momentum
      m0 = circ0.mass 
      (cx1, cy1) = circ1.center 
      (px1, py1) = circ1.momentum
      m1 = circ1.mass 
      (nx, ny) = (cx1 - cx0, cy1 - cy0) 
      ((px0', py0'), (px1', py1')) = 
        exchangeMomenta (nx, ny) (m0, (px0, py0)) (m1, (px1, py1)) 
      opt0 = ((Circle circ0), (Circle circ1)) 
      optE = let circ0' = {circ0 | momentum <- (px0', py0')} 
                 circ1' = {circ1 | momentum <- (px1', py1')} 
              in ((Circle circ0'), (Circle circ1'))
      opts = [opt0, optE]
      mts = L.map (uncurry esct) opts
   in if (L.all isNothing mts) 
      then opt0 
      else let oes = L.map2 (,) opts mts
               oes' = oes |> L.filter (isJust << snd) 
                          |> L.map (\(opt, Just t) -> (opt, t)) 
                          |> L.filter (\(_, t) -> t > 0.0)
                          |> L.sortBy snd
            in maybe opt0 fst (L.head oes')

interactCircle : Circle -> Circle -> (Circle, Circle)
interactCircle = interactEscCircle escapeTimeCircle

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
scatterCircle : List Circle -> List Circle
scatterCircle = scatter isOverlapCircle interactCircle 

-- define a type to unite the signals from Time and Window.dimensions
type TW = TW Time.Time (Int, Int)

stepRaw : TW -> List Circle -> List Circle 
stepRaw (TW dt (m, n)) circles = 
  L.map ((evolveCircle dt) << (reflectCircle (m, n))) (scatterCircle circles)

-- if the computer returns from a sleeping mode the dt can be too large 
-- maxDelta defines a cut-off to avoid a huge dt
step : TW -> List Circle -> List Circle 
step (TW dt (m, n) as tw) circles = 
  if (dt > maxDelta) 
  then circles
  else stepRaw tw circles

----------

mySignal : Signal TW
mySignal = sampleOn myFps (TW <~ myFps ~ Window.dimensions)

main : Signal Element
main = view <~ Window.dimensions ~ foldp step initialConfig mySignal

----------

-- The test configuration. 

initializeCircle : (Float, Float) 
                -> (Float, Float) 
                -> Float 
                -> Float 
                -> Color 
                -> Circle
initializeCircle (cx, cy) (px, py) m r c = 
  Circle { center = (cx, cy)
         , momentum = (px, py)
         , mass = defaultMass
         , radius = r 
         , color = c
         }

defaultRadius : Float
defaultRadius = 80.0

defaultMass : Float
defaultMass = 1.0 

initCircle : (Float, Float) -> (Float, Float) -> Color -> Circle
initCircle (cx, cy) (px, py) c = 
  initializeCircle (cx, cy) (px, py) defaultMass defaultRadius c

initialConfig : List Circle
initialConfig = [ initCircle (400.0, 100.0) (0.0, 0.0) darkBlue
                , initCircle (-150.0, 70.0) (0.4, 0.4) darkGreen
                , initCircle (300.0, -200.0) (0.6, -0.5) darkRed
                , initCircle (-100.0, -200.0) (0.7, -0.8) darkYellow
                , initCircle (-450.0, 0.0) (-0.35, 0.25) darkOrange
                , initCircle (-420.0, -300.0) (0.3, 0.7) lightBlue
                , initCircle (150.0, 200.0) (-0.3, 0.45) darkPurple 
                , initCircle (-420.0, 280.0) (-0.7, -0.15) lightBrown
                ]
