module Model.Goal exposing (..)

import Model.Flower exposing (..)

import Html5.DragDrop as DnD


-- Selection


type alias Selection
  = List Zipper


-- Drag-and-Drop


type alias FlowerDragId
  = { source : Zipper, content : Flower }

type alias FlowerDropId
  = Maybe { target : Zipper, content : Bouquet }

type alias FlowerDnD
  = DnD.Model FlowerDragId FlowerDropId

type alias FlowerDnDMsg
  = DnD.Msg FlowerDragId FlowerDropId


-- Modal UI


type ProofInteraction
  = Justifying
  | Importing
  | Fencing Selection


type alias Surgery =
  { cropped : Maybe Flower
  , pulled : Maybe Garden }


initialSurgery : Surgery
initialSurgery =
  { cropped = Nothing
  , pulled = Nothing }


type EditInteraction
  = Operating
  | Adding Zipper
  | Reordering


type UIMode
  = ProofMode ProofInteraction
  | EditMode EditInteraction Surgery
  | NavigationMode


{- A Goal is made of the following data:

   focus: the bouquet that the user is currently working on
   context: the context in which the bouquet occurs
   mode: the current mode of interaction
   dragDrop: the state of an eventual ongoing drag-and-drop operation
-}

type alias Goal
  = { focus : Bouquet
    , context : Context
    , mode : UIMode
    , dragDrop : FlowerDnD
    }


fromBouquet : Bouquet -> Goal
fromBouquet bouquet =
  { focus = bouquet
  , context = Context [] Pos
  , mode = ProofMode Justifying
  , dragDrop = DnD.init
  }


map : (Bouquet -> Bouquet) -> Goal -> Goal
map f goal =
  { goal | focus = f goal.focus }