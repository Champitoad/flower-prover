module Model.Goal exposing (..)

import Model.Flower as Flower exposing (..)

import Dict exposing (Dict)


-- Selection


type alias Selection
  = List Zipper


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
   location: a unique, semantic identifier for the goal location
   mode: the current mode of interaction
-}


type Location
  = App
  | Manual SandboxID


type alias Goal
  = { focus : Bouquet
    , context : Context
    , location : Location
    , mode : UIMode
    }


fromBouquet : Bouquet -> Goal
fromBouquet bouquet =
  { focus = bouquet
  , context = Context [] Pos
  , location = App
  , mode = ProofMode Justifying
  }


map : (Bouquet -> Bouquet) -> Goal -> Goal
map f goal =
  { goal | focus = f goal.focus }


-- A Sandbox is a Goal that can be reset


type alias Sandbox =
    { initialGoal : Goal
    , currentGoal : Goal
    }

type alias SandboxID = String

type alias Sandboxes = Dict SandboxID Sandbox


mkSandbox : Goal -> Sandbox
mkSandbox goal =
  { initialGoal = goal
  , currentGoal = goal
  }


getSandbox : SandboxID -> Sandboxes -> Sandbox
getSandbox id sandboxes =
  case Dict.get id sandboxes of
    Nothing ->
      let _ = Debug.log "Warning" "trying to retrieve a non-existing sandbox; returning a dummy one." in
      mkSandbox (fromBouquet [])

    Just sandbox ->
      sandbox


updateSandbox : SandboxID -> Goal -> Sandboxes -> Sandboxes
updateSandbox id goal sandboxes =
  case Dict.get id sandboxes of
    Nothing ->
      let _ = Debug.log "Warning" "trying to update a non-existing sandbox. Ignoring." in
      sandboxes

    Just sandbox ->
      let
        updatedSandbox =
          { initialGoal = sandbox.initialGoal
          , currentGoal = goal
          }
      in
      Dict.insert id updatedSandbox sandboxes


resetSandbox : SandboxID -> Sandboxes -> Sandboxes
resetSandbox id sandboxes =
  case Dict.get id sandboxes of
    Nothing ->
      let _ = Debug.log "Warning" "trying to reset a non-existing sandbox. Ignoring." in
      sandboxes

    Just sandbox ->
      updateSandbox id sandbox.initialGoal sandboxes


manualExamples : Sandboxes
manualExamples =
  let
    makeSandbox id mode bouquet =
      mkSandbox
        { focus = bouquet
        , context = Context [] Pos
        , location = Manual id
        , mode = mode
        }
    
    examples : List (SandboxID, UIMode, Bouquet)
    examples =
      [ ( "Flower", ProofMode Justifying, [makeFlower [a"a", a"b"] [[a"c"], [a"d"]]] )
      , ( "QED", ProofMode Justifying, [makeFlower [a"a"] [[]]] )
      , ( "Justify", ProofMode Justifying, [Flower.identity] )
      , ( "Modus Ponens", ProofMode Justifying, [Flower.modusPonensCurryfied] )
      ]
  in
  examples |>
  List.map (\(id, mode, bouquet) -> (id, makeSandbox id mode bouquet)) |>
  Dict.fromList