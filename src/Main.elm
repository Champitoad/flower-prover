port module Main exposing (..)

import Formula exposing (..)
import Flower exposing (..)

import Utils.List
import Utils.Events exposing (..)
import Utils.Color as Color

import Browser

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events

import Html exposing (Html)
import Html.Attributes exposing (style, title)

import Html5.DragDrop as DnD

import Json.Decode exposing (Value)      

import FeatherIcons as Icons


-- MAIN


port dragstart : Value -> Cmd msg


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none      


-- MODEL


--- Selection


type alias Selection
  = List Zipper


--- Drag-and-Drop


type alias FlowerDragId
  = { source : Zipper, content : Flower }

type alias FlowerDropId
  = Maybe { target : Zipper, content : Bouquet }

type alias FlowerDnD
  = DnD.Model FlowerDragId FlowerDropId

type alias FlowerDnDMsg
  = DnD.Msg FlowerDragId FlowerDropId


--- Modal UI


type ProofInteraction
  = Justifying
  | Importing
  | Fencing Selection


type EditInteraction
  = Erasing
  | Adding Zipper
  | Reordering


type UIMode
  = ProofMode ProofInteraction
  | EditMode EditInteraction
  | NavigationMode


--- Full application state


type alias Model
  = { goal : Bouquet
    , mode : UIMode
    , dragDrop : FlowerDnD
    , history : History }


init : () -> (Model, Cmd Msg)
init () =
  ( { goal = [ orElimInvertible ]
    , mode = EditMode Erasing
    , dragDrop = DnD.init
    , history = History { prev = Nothing, next = Nothing } }
  , Cmd.none )


---- History of the full state mutually defined


type History
  = History { prev : Maybe Model
            , next : Maybe Model }


getHistory : Model ->  { prev : Maybe Model, next : Maybe Model }
getHistory model =
  let (History history) = model.history in
  history


setHistory : { prev : Maybe Model, next : Maybe Model } -> Model -> Model
setHistory history model =
  { model | history = History history }


undo : Model -> Model
undo model =
  case (getHistory model).prev of
    Just prevModel ->
      let prevHistory = getHistory prevModel in
      setHistory { prevHistory | next = Just model } prevModel
    Nothing ->
      model


redo : Model -> Model
redo model =
  case (getHistory model).next of
    Just nextModel ->
      nextModel
    Nothing ->
      model


-- UPDATE


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


-- VIEW


---- Common styling


transparent : Color
transparent =
  rgba 0 0 0 0


flowerForegroundColor : Polarity -> Color
flowerForegroundColor polarity =
  case polarity of
    Pos ->
      rgb 0 0 0
    Neg ->
      rgb 1 1 1

flowerBackgroundColor : Polarity -> Color
flowerBackgroundColor polarity =
  case polarity of
    Pos ->
      rgb 1 1 1
    Neg ->
      rgb 0 0 0


flowerBorderWidth : Int
flowerBorderWidth =
  3

flowerBorderRound : Int
flowerBorderRound =
  10


type alias ZoneStyle msg
  = { borderWidth : Int
    , active : List (Attribute msg)
    , inactive : List (Attribute msg) }


actionable : Color.Color -> ZoneStyle msg
actionable color =
  let
    width =
      3

    border =
      [ Border.width 5
      , Border.dotted
      , Border.rounded flowerBorderRound ]
    
    bgColor =
      Color.withAlpha 0.5 color |> Color.toElement
  in
  { borderWidth = width
  , active =
      pointer ::
      Border.color (Color.toElement color) ::
      Background.color bgColor ::
      border
  , inactive =
      Border.color transparent ::
      border }


greenActionable : ZoneStyle msg
greenActionable =
  actionable (Color.fromRgb { red = 0.3, green = 0.9, blue = 0.3 })

pinkActionable : ZoneStyle msg
pinkActionable =
  actionable (Color.fromRgb { red = 1, green = 0.1, blue = 0.8 })

orangeActionable : ZoneStyle msg
orangeActionable =
  actionable (Color.fromRgb { red = 1, green = 0.6, blue = 0 })


droppable : Color.Color -> ZoneStyle msg
droppable color =
  let
    width =
      3

    border =
      [ Border.width width
      , Border.dashed
      , Border.rounded flowerBorderRound
      , Border.color (Color.toElement color) ]
    
    bgColor =
      Color.withAlpha 0.5 color |> Color.toElement
  in
  { borderWidth = width
  , active = Background.color bgColor :: border
  , inactive = border }


nonSelectable : Attribute Msg
nonSelectable =
  htmlAttribute <| style "user-select" "none"


---- Common events


stopPropagation : List (Attribute Msg)
stopPropagation =
  [ onDragOver DoNothing
  , onMouseMove DoNothing ]


dragAction : Zipper -> Flower -> List (Attribute Msg)
dragAction zipper flower =
  if List.length zipper <= 1 then []
  else
    List.map htmlAttribute <|
    DnD.draggable DragDropMsg
      { source = zipper, content = flower }


---- Goal


viewFormula : Model -> Context -> Formula -> Element Msg
viewFormula model context formula =
  let
    form =
      Formula formula

    clickAction =
      let
        actionableStyle =
          case formula of
            Atom _ -> greenActionable
            _ -> pinkActionable
      in
      case model.mode of
        ProofMode Justifying ->
          case formula of
            Atom _ ->
              if isHypothesis form context.zipper then
                (Events.onClick (Action Justify [form] context.zipper))
                :: (htmlAttribute <| title "Justify")
                :: actionableStyle.active
              else
                actionableStyle.inactive
            
            _ ->
              (Events.onClick (Action Decompose [form] context.zipper))
                :: (htmlAttribute <| title "Decompose")
              :: actionableStyle.active

        _ ->
          actionableStyle.inactive
    
    reorderDragAction =
      case model.mode of
        EditMode _ ->
          dragAction context.zipper form
        
        _ ->
          []
  in
  el
    ( [ centerX, centerY
      , padding 10
      , Font.color (flowerForegroundColor context.polarity)
      , Font.size 50
      , nonSelectable ]
      ++ clickAction
      ++ reorderDragAction )
    ( text (Formula.toString formula) )


viewPistil : Model -> Context -> Garden -> List Garden -> Element Msg
viewPistil model context (Garden bouquet as pistil) petals =
  let
    newZipper =
      Pistil petals :: context.zipper

    clickAction =
      let actionableStyle = orangeActionable in
      case model.mode of
        ProofMode Justifying ->
          let
            action name =
              (Events.onClick (Action Unlock bouquet newZipper))
              :: (htmlAttribute <| title name)
              :: actionableStyle.active
          in
          if List.isEmpty bouquet then
            case context.zipper of
              _ :: Pistil _ :: _ ->
                let
                  name =
                    case List.length petals of
                      0 -> "Ex falso quodlibet"
                      1 -> "Unlock"
                      _ -> "Case"
                in
                action name
              _ ->
                if List.length petals == 1 then
                  action "Unlock"
                else
                  actionableStyle.inactive
          else
            actionableStyle.inactive
        
        _ ->
          actionableStyle.inactive
  in
  el
    ( [ width fill
      , height fill
      , Border.rounded flowerBorderRound ] )
    ( el
        ( [ width fill
          , height fill
          , padding 10 ]
         ++ clickAction )
        ( viewGarden
            model
            { context
            | zipper = newZipper
            , polarity = invert context.polarity }
            pistil ) )


viewPetal : Model -> Context -> Garden -> (List Garden, List Garden) -> Garden -> Element Msg
viewPetal model context pistil (leftPetals, rightPetals) (Garden bouquet as petal) =
  let
    newZipper =
      Petal pistil leftPetals rightPetals :: context.zipper

    clickAction =
      let actionableStyle = greenActionable in
      case model.mode of
        ProofMode Justifying ->
          if List.isEmpty bouquet then
            (Events.onClick (Action Close bouquet newZipper))
            :: (htmlAttribute <| title "QED")
            :: actionableStyle.active
          else
            actionableStyle.inactive
        
        _ ->
          actionableStyle.inactive
  in
  el
    [ width fill
    , height fill
    , Border.rounded flowerBorderRound
    , Background.color (flowerBackgroundColor context.polarity) ]
    ( el
        ( [ width fill
          , height fill
          , padding 10 ]
         ++ clickAction )
        ( viewGarden
            model
            { context
            | zipper = newZipper }
            petal ) )


viewFlower : Model -> Context -> Flower -> Element Msg
viewFlower model context flower =
  case flower of
    Formula formula ->
      viewFormula model context formula
    
    Flower pistil petals ->
      let
        pistilEl =
          viewPistil model context pistil petals
        
        petalsEl =
          row
            [ width fill
            , height fill
            , spacing flowerBorderWidth ]
            ( Utils.List.zipperMap (viewPetal model context pistil) petals )
      in
      column
        ( [ width fill
          , height fill
          , padding flowerBorderWidth
          , Background.color (flowerForegroundColor context.polarity)
          , Border.color (flowerForegroundColor context.polarity)
          , Border.rounded flowerBorderRound
          , Border.shadow
              { offset = (0, 5)
              , size = 0.25
              , blur = 15
              , color = flowerForegroundColor context.polarity } ]
        ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
        ++ dragAction context.zipper flower )
        [ pistilEl, petalsEl ]


viewGarden : Model -> Context -> Garden -> Element Msg
viewGarden model context (Garden bouquet) =
  let
    flowerEl (left, right) =
      viewFlower
        model
        { context
        | zipper = Bouquet left right :: context.zipper }
    
    dropAction (left, right) =
      case model.mode of
        ProofMode Importing ->
          case DnD.getDragId model.dragDrop of
            Just { source } ->
              -- if isHypothesis content context.zipper then
              if justifies source context.zipper then
                let
                  dropStyle =
                    droppable (Color.fromRgb { red = 1, green = 0.8, blue = 0 })

                  dropTargetStyle =
                    case DnD.getDropId model.dragDrop of
                      Just (Just { target }) ->
                        if Bouquet left right :: context.zipper == target
                        then dropStyle.active
                        else dropStyle.inactive
                    
                      _ ->
                        dropStyle.inactive
                in
                dropTargetStyle ++
                ( List.map htmlAttribute <|
                  DnD.droppable DragDropMsg
                    (Just { target = Bouquet left right :: context.zipper
                          , content = [] }) )
              else
                []
            _ ->
              []

        EditMode Reordering ->
          case DnD.getDragId model.dragDrop of
            Just { source, content } ->
              case source of
                Bouquet sourceLeft sourceRight :: sourceParent ->
                  if sourceParent == context.zipper then
                    let
                      (sourceIndex, index) =
                        (List.length sourceLeft, List.length left)
                    in
                    if sourceIndex == index || index == sourceIndex + 1 then []
                    else
                      let
                        whole =
                          left ++ right

                        newBouquet =
                          if sourceIndex < index then
                            let
                              middle =
                                Utils.List.slice (sourceIndex + 1) (index - 1) whole
                            in
                            sourceLeft ++ middle ++ content :: right
                          else
                            let
                              middle =
                                Utils.List.slice index (sourceIndex - 1) whole
                            in
                            left ++ content :: (middle ++ sourceRight)

                        dropStyle =
                          droppable (Color.fromRgb { red = 0.7, green = 0.7, blue = 0.7 })

                        dropTargetStyle =
                          case DnD.getDropId model.dragDrop of
                            Just (Just { target }) ->
                              if Bouquet left right :: context.zipper == target
                              then dropStyle.active
                              else dropStyle.inactive

                            _ ->
                              dropStyle.inactive
                      in
                      dropTargetStyle ++
                      ( List.map htmlAttribute <|
                        DnD.droppable DragDropMsg
                          (Just { target = Bouquet left right :: context.zipper
                                , content = newBouquet }) )
                  else
                    []
                _ ->
                  []
            _ ->
              []
        _ ->
          []

    spaceSize = 30
    
    layoutAttrs =
      [ (width (fill |> minimum spaceSize))
      , (height (fill |> minimum spaceSize))
      , padding spaceSize
      , spacing spaceSize ]
    
    borderAttrs =
      [ Border.width (droppable Color.transparent).borderWidth
      , Border.color transparent ]

    length flower =
      case flower of
        Formula _ -> shrink
        Flower _ _ -> fill

    intersticial () =
      let
        attrs =
          layoutAttrs ++
          borderAttrs

        dropZone lr =
          el
            ( [ width (spaceSize |> px)
              , height (fill |> minimum spaceSize) ]
            ++ borderAttrs
            ++ dropAction lr )
            none

        sperse ((left, right) as lr) flower =
          let
            lastDropzone =
              if List.isEmpty right
              then [onRight (dropZone (left ++ [flower], right))]
              else []
          in
          el
            ( [ width (length flower)
              , height fill
              , centerX, centerY
              , onLeft (dropZone (left, flower :: right)) ]
              ++ lastDropzone )
            ( flowerEl lr flower )

        els =
          Utils.List.zipperMap sperse bouquet
      in
      wrappedRow attrs els
        
    normal () =
      let
        attrs =
          layoutAttrs ++
          borderAttrs ++
          dropAction (bouquet, [])
        
        sperse lr flower =
          el
            [ width (length flower)
            , height fill
            , centerX, centerY ]
            ( flowerEl lr flower )

        els =
          Utils.List.zipperMap sperse bouquet
      in
      wrappedRow attrs els
  in
  case model.mode of
    EditMode _ ->
      intersticial ()

    _ ->
      normal ()


viewGoal : Model -> Element Msg
viewGoal model =
  let
    bouquetEls () =
      ( Utils.List.zipperMap
          ( \(l, r) flower ->
              el [ width fill, height fill, centerX, centerY ]
              ( viewFlower model (Context [Bouquet l r] Pos) flower ) )
          model.goal )
    
    bouquetEl () =
      column
        [ width fill
        , height fill
        , scrollbarY
        , spacing 100
        , Background.color (rgb 0.65 0.65 0.65) ]
        ( bouquetEls () )

    workingOnIt =
      el
        [ width fill
        , height fill
        , Background.color (rgb 1 1 1) ]
        ( el
            [ centerX, centerY
            , Font.size 50 ]
            ( text "Working on it!" ) )
  in
  case model.mode of
    ProofMode _ ->
      bouquetEl ()
    
    EditMode _ ->
      bouquetEl ()

    _ ->
      workingOnIt


---- Toolbar


type ModeSelectorPosition
  = Start | Middle | End


viewModeSelector : UIMode -> Element Msg
viewModeSelector currentMode =
  let
    borderRoundSize = 10

    item mode position =
      let
        isSelected =
          case (mode, currentMode) of          
            (ProofMode _, ProofMode _) -> True
            (EditMode _, EditMode _) -> True
            _ -> mode == currentMode

        (bgColor, fgColor) =
          if isSelected
          then (rgb255 58 134 255, rgb 1 1 1)
          else (rgb 1 1 1, rgb 0 0 0)

        (iconEl, titleText) =
          let
            (title, icon) =
              case mode of
                ProofMode _ -> ("Prove", Icons.checkSquare)
                EditMode _ -> ("Edit", Icons.edit2)
                NavigationMode -> ("Navigate", Icons.navigation)
            elem =
              el
                [ centerX, centerY ]
                ( icon
                  |> Icons.withSize 30
                  |> Icons.toHtml [ fgColor |> Color.fromElement |> Color.toHtml ]
                  |> html )
          in
          (elem, title)
        
        borderRound =
          case position of
            Start ->
              { topLeft = borderRoundSize, bottomLeft = borderRoundSize
              , topRight = 0, bottomRight = 0 }
            Middle ->
              { topLeft = 0, bottomLeft = 0
              , topRight = 0, bottomRight = 0 }
            End ->
              { topLeft = 0, bottomLeft = 0
              , topRight = borderRoundSize, bottomRight = borderRoundSize }

        changeAction =
          [ Events.onClick (ChangeUIMode mode)
          , pointer ]
      in
      el
        ( [ width (60 |> px)
          , height (55 |> px)
          , Background.color bgColor
          , Border.roundEach borderRound
          , htmlAttribute <| title titleText ]
         ++ changeAction )
        iconEl
    
    borderColor = rgb 0.6 0.6 0.6
  in
  row
    [ width shrink
    , height fill
    , centerX, centerY
    , spacing 1
    , Border.width 0
    , Border.rounded borderRoundSize
    , Border.color borderColor
    , Background.color borderColor ]
    [ item (ProofMode Justifying) Start
    , item (EditMode Erasing) Middle
    , item NavigationMode End ]


viewUndoRedo : Element Msg
viewUndoRedo =
  let
    undoAction =
      [Events.onClick Undo]
  in
  row
    [ width shrink
    , height shrink ]
    [ el
        ( [ width (60 |> px)
          , height (60 |> px) ]
         ++ undoAction )
        ( Icons.arrowLeftCircle
          |> Icons.withSize 30
          |> Icons.toHtml []
          |> html ) ]


viewToolbar : Model -> Element Msg
viewToolbar model =
  row
    [ width fill
    , height shrink
    , padding 15
    , Background.color (rgb 0.9 0.9 0.9) ]
    [ viewModeSelector model.mode
    , viewUndoRedo ]


---- Full application


view : Model -> Html Msg
view model =
  let
    goal =
      viewGoal model
    
    toolbar =
      viewToolbar model
    
    app =
      column
        [ width fill,
          height fill ]
        [ goal, toolbar ]
  in
  layout [] app