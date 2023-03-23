port module Update.App exposing (..)

import Model.Flower exposing (..)
import Model.App exposing (..)

import Json.Decode exposing (Value)      

import Html5.DragDrop as DnD


port dragstart : Value -> Cmd msg


type Rule
  = Decompose -- introduction of connective
  | Justify -- down pollination
  | Import -- up pollination
  | Unlock -- empty pistil
  | Close -- empty petal
  | Fence -- fencing
  | Reorder -- multiset


type Msg
  = Action Rule Bouquet Zipper
  | DragDropMsg FlowerDnDMsg
  | ChangeUIMode UIMode
  | Undo
  | Redo
  | DoNothing


applyAction : Rule -> Bouquet -> Zipper -> Bouquet
applyAction rule bouquet zipper =
  case (rule, bouquet, zipper) of
    (Decompose, [Formula formula], _) ->
      fillZipper (decompose formula) zipper

    (Justify, _, _) ->
      fillZipper [] zipper
    
    (Import, _, _) ->
      fillZipper bouquet zipper

    (Unlock, [], Pistil [Garden petal] :: parent)  ->
      fillZipper petal parent
    
    (Unlock, [], Pistil branches :: Bouquet left right :: Pistil petals :: parent) ->
      let
        case_ : Garden -> Flower
        case_ branch =
          Flower branch petals
        
        pistil =
          Garden (left ++ right)
        
        cases =
          List.map case_ branches  
      in
      fillZipper [Flower pistil [Garden cases]] parent
    
    (Close, [], Petal _ _ _ :: parent) ->
      fillZipper [] parent
    
    (Reorder, _, _ :: parent) ->
      fillZipper bouquet parent

    _ ->
      Debug.todo "Unsupported action"


handleDragDropMsg : FlowerDnDMsg -> Model -> (Model, Cmd Msg)
handleDragDropMsg dndMsg model =
  let
    dragStart = 
      DnD.getDragstartEvent dndMsg

    cmd =
      dragStart
      |> Maybe.map (.event >> dragstart)
      |> Maybe.withDefault Cmd.none

    ( newDragDrop, result ) =
      DnD.update dndMsg model.dragDrop

    model_ =
      case dragStart of
        Just _ ->
          case model.mode of
            ProofMode Justifying ->
              { model | dragDrop = newDragDrop, mode = ProofMode Importing }

            EditMode Erasing ->
              { model | dragDrop = newDragDrop, mode = EditMode Reordering }
            
            _ ->
              model

        Nothing ->
          let
            defaultMode =
              case model.mode of
                ProofMode _ -> ProofMode Justifying
                EditMode _ -> EditMode Erasing
                _ -> model.mode
          in
          case result of
            Just (drag, drop, _) ->
              case drop of
                -- Dropping on target
                Just destination ->
                  let
                    action =
                      case model.mode of
                        ProofMode Importing ->
                          Action Import
                            [drag.content] destination.target
                        
                        EditMode Reordering ->
                          Action Reorder
                            destination.content destination.target
                        
                        _ ->
                          DoNothing
                    
                    newModel =
                      update action model |> Tuple.first
                  in
                  { newModel | dragDrop = newDragDrop, mode = defaultMode }

                -- Dropping on non-target
                Nothing ->
                  { model | dragDrop = newDragDrop, mode = defaultMode }
        
            -- Dragging
            Nothing ->
              { model | dragDrop = newDragDrop }
  in
  (model_, cmd)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Action rule bouquet zipper ->
      ( { model
        | goal = applyAction rule bouquet zipper
        , history = History { prev = Just model, next = Nothing } }
      , Cmd.none )

    DragDropMsg dndMsg ->
      handleDragDropMsg dndMsg model
    
    ChangeUIMode mode ->
      ({ model | mode = mode }, Cmd.none)
    
    Undo ->
      (undo model, Cmd.none)

    Redo ->
      (redo model, Cmd.none)
    
    DoNothing ->
      (model, Cmd.none)