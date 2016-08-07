-- Filename: bose-fermi.elm

-- Language: Elm 0.16
-- You can run this code at http://elm-lang.org/try

-- Author: artuuge@gmail.com

-- This is an extension of the example checkbox.elm to many boxes. 
-- The new aspect is that the boxes can follow either "Bose statistics" 
-- (the check-box case), or the "Fermi statistics" (the radio-button case). 
-- Bose: any number of boxes can be selected. 
-- Fermi: at most one box can be selected. 

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input as I 
import List as L
import Signal exposing (..)
import Text as T

-- One needs to label the boxes in the collection (the cluster). 
type alias Label = Int

-- Fermi corresonds to radio-buttons 
-- Bose corresponds to check-boxes
type Kind = Fermi | Bose

type alias State = { width : Int
                   , height : Int  
                   , selected : Bool
                   , excited : Bool 
                   }

-- extension over Label, tensor product is a functor
type alias Box = { label : Label, state : State } 

-- List is a funtor and tensor product (extension over Kind) is a functor
-- The collection of boxes is termed a cluster. 
type alias Cluster = { kind : Kind, boxes : List Box }

-- Define the types for the step combinators. 
-- stepState: Fact -> State -> State
-- stepBox : Event -> Box -> Box
-- stepCluster : Action -> Cluster -> Cluster
-- (State/Fact) => (Box/Event) => (Cluster/Action)
type Fact = CC Bool | HH Bool Bool
type Event = Click Label | Hover Label Bool 
type Action = E Event | Reset Kind
-- There is a canonical monomorphism E : Event >--> Action.
-- Every (m : Label) yiels an epimorphism (transfer m) : Event -->> Fact.
-- Event = Event1 * Label, where Event1 = Click1 | Hover1 Bool.
-- Action = (Reset Kind) + Event1 * Label.  
-- Informally: the type Action is obtained via a linear transformation 
-- "y = a x + b" from its counterpart in the single box case.   
-- The view and step combinators defined below are obtained, essentially, 
-- by funtoriality. 

type alias Report = { kind : Kind, labels : List Label } 
-- observation of the cluster 

getLabel : Box -> Label 
getLabel box = box.label

isSelected : Box -> Bool 
isSelected box = box.state.selected

-- the measuring device (observation)
reportSelected : Cluster -> Report 
reportSelected cluster = 
  { kind = cluster.kind
  , labels = L.map getLabel (L.filter isSelected cluster.boxes) 
  } 

displayReport : Report -> Element 
displayReport report = 
  flow right [  show "Current selection: "
             , show report
             ]

initialState : State
initialState = { width = 80
               , height = 60
               , selected = False
               , excited = False 
               }
-- hardwire the geometry

initBox : Label -> Box 
initBox label = { label = label, state = initialState }

initCluster : Kind -> List Label -> Cluster 
initCluster kind labels = { kind = kind, boxes = L.map initBox labels }

displayState : State -> Element
displayState state = 
  let (w, h) = (state.width, state.height) 
      (s, e) = (state.selected, state.excited) 
      c = case (s, e) of 
        (False, False) -> lightPurple
        (False, True)  -> lightBlue
        (True, False)  -> darkRed
        (True, True)   -> darkOrange
  in collage w h [ rect (toFloat w) (toFloat h) |> filled c ]
-- hardwire the choice of colors

viewBox : Mailbox Action -> Box -> Element
viewBox chan box = 
  displayState box.state 
    |> I.hoverable (message chan.address << E << Hover box.label) 
    |> I.clickable (message chan.address << E <| Click box.label)
-- the actions which a box can emit are in the monomorphic image of Event


viewClusterPic : Mailbox Action -> Cluster -> Element 
viewClusterPic chan cluster = 
    flow right <| L.map (viewBox chan) cluster.boxes

viewClusterReport : Cluster -> Element 
viewClusterReport = displayReport << reportSelected 

viewClusterButtons : Mailbox Action -> Element 
viewClusterButtons chan = 
  flow right 
    [ I.button (message chan.address (Reset Bose)) "Bose" 
    , I.button (message chan.address (Reset Fermi)) "Fermi"
    ]

viewCluster : Mailbox Action -> Cluster -> Element 
viewCluster chan cluster = 
  flow down 
    [ viewClusterPic chan cluster 
    , viewClusterReport cluster
    , viewClusterButtons chan 
    ] 

stepState : Kind -> Fact -> State -> State 
stepState kind fact state = case kind of 
  Fermi -> case fact of 
    CC p       -> { state | selected = p } 
    HH True p  -> {state | excited = p } 
    HH False _ -> state
  Bose -> case fact of 
    CC True    -> { state | selected = (not state.selected) } 
    CC False   -> state 
    HH True p  -> {state | excited = p } 
    HH False _ -> state
-- this combinator defines the logic of the cluster 

transfer : Label -> Event -> Fact 
transfer n event = case event of 
  Click m   -> CC (m == n) 
  Hover m p -> HH (m == n) p
-- connection over a bundle, Label is the base

stepBox : Kind -> Event -> Box -> Box 
stepBox kind event box = 
  { box | state = 
    stepState kind (transfer box.label event) box.state }

defaultLabels : List Int 
defaultLabels = [0 .. 7]
-- hardwire the size of the cluster

stepCluster : Action -> Cluster -> Cluster 
stepCluster action cluster = case action of 
  Reset kind -> initCluster kind defaultLabels
  E event -> 
    { cluster | boxes = 
      L.map (stepBox cluster.kind event) cluster.boxes} 

defaultKind : Kind 
defaultKind = Bose
-- hardwire the starting kind of the cluster

actions : Mailbox Action 
actions = mailbox (Reset defaultKind)

main : Signal Element
main = map (viewCluster actions) (foldp stepCluster 
         (initCluster defaultKind defaultLabels) actions.signal)
