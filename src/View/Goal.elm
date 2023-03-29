module View.Goal exposing (..)

import View.Style as Style exposing (..)
import View.Widgets exposing (..)
import View.Events exposing (..)

import Model.Formula as Formula exposing (..)
import Model.Flower exposing (..)
import Model.App exposing (..)

import Update.Rules exposing (..)
import Update.App exposing (..)

import Utils.List
import Utils.Events
import Utils.Color

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font

import Html.Attributes exposing (title)

import Html5.DragDrop as DnD

import Css

import FeatherIcons as Icons

import Color


reorderColor : Color.Color
reorderColor =
  Utils.Color.fromRgb { red = 0.7, green = 0.7, blue = 0.7 }


importColor : Color.Color
importColor =
  Utils.Color.fromRgb { red = 1, green = 0.8, blue = 0 }


cropAction : Surgery -> Context -> Flower -> List (Attribute Msg)
cropAction surgery context flower =
  let actionableStyle = redActionable in
  if croppable surgery context then
    Utils.Events.onClick (Action Crop context.zipper [flower])
    :: (htmlAttribute <| title "Remove Flower")
    :: actionableStyle.active
  else
    actionableStyle.inactive


pullAction : Surgery -> Context -> Bouquet -> List (Attribute Msg)
pullAction surgery context bouquet =
  let actionableStyle = redActionable in
  if pullable surgery context then
    Utils.Events.onClick (Action Pull context.zipper bouquet)
    :: (htmlAttribute <| title "Remove Petal")
    :: actionableStyle.active
  else
    actionableStyle.inactive


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
                (Events.onClick (Action Justify context.zipper [form]))
                :: (htmlAttribute <| title "Justify")
                :: actionableStyle.active
              else
                actionableStyle.inactive
            
            _ ->
              (Events.onClick (Action Decompose context.zipper [form]))
              :: (htmlAttribute <| title "Decompose")
              :: actionableStyle.active
        
        EditMode Operating surgery ->
          cropAction surgery context (Formula formula)

        _ ->
          actionableStyle.inactive
    
    reorderDragAction =
      case model.mode of
        EditMode _ _ ->
          dragAction reorderColor model.dragDrop context.zipper form
        
        _ ->
          []
  in
  el
    ( [ centerX, centerY
      , padding 10
      , Font.color (flowerForegroundColor context.polarity)
      , Font.size 50
      , nonSelectable ]
      ++ reorderDragAction
      ++ clickAction )
    ( text (Formula.toString formula) )


viewPistil : Model -> Context -> Garden -> List Garden -> Element Msg
viewPistil model context (Garden bouquet as pistil) petals =
  let
    newZipper =
      Pistil petals :: context.zipper

    clickAction =
      case model.mode of
        ProofMode Justifying ->
          let
            actionableStyle = orangeActionable

            action rule name =
              (Events.onClick (Action rule newZipper bouquet))
              :: (htmlAttribute <| title name)
              :: actionableStyle.active
          in
          if List.isEmpty bouquet then
            case context.zipper of
              _ :: Pistil _ :: _ ->
                let
                  (rule, name) =
                    case List.length petals of
                      0 -> (Close, "Ex falso quodlibet")
                      1 -> (Unlock, "Unlock")
                      _ -> (Case, "Case")
                in
                action rule name
              _ ->
                if List.length petals == 1 then
                  action Unlock "Unlock"
                else
                  actionableStyle.inactive
          else
            actionableStyle.inactive
        
        EditMode Operating surgery ->
          cropAction surgery context (Flower pistil petals)

        _ ->
          (actionable Utils.Color.transparent).inactive
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
            (Events.onClick (Action Close newZipper bouquet))
            :: (htmlAttribute <| title "QED")
            :: actionableStyle.active
          else
            actionableStyle.inactive
        
        EditMode Operating surgery ->
          pullAction surgery { context | zipper = newZipper } bouquet

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


addButton : ButtonParams msg -> Element msg
addButton params =
  let
    style =
      { width = Css.px defaultButtonSize
      , height = Css.pct 100
      , color = Color.rgb255 58 134 255
      , iconColorEnabled = Color.white
      , iconColorDisabled = Color.darkGray }
  in
  button style params


viewAddPetalZone : Context -> Garden -> List Garden -> Element Msg
viewAddPetalZone context pistil petals =
  let
    newFlower =
      Flower pistil (petals ++ [Garden []])

    addPetalButton =
        ( addButton
            { msg =  Action Grow context.zipper [newFlower]
            , title = "Add Petal"
            , icon = Icons.plus
            , enabled = True } )
  in
  column
    [ width shrink
    , height fill
    , padding 10
    , Background.color (flowerBackgroundColor (invert context.polarity)) ]
    [ addPetalButton ]


viewAddFlowerZone : Context -> Bouquet -> Element Msg
viewAddFlowerZone context bouquet =
  let
    newFlower =
      yinyang

    newZipper =
      Bouquet bouquet [] :: context.zipper

    addFlowerButton =
      el
        [ width shrink
        , height fill ]
        ( addButton
            { msg = Action Grow newZipper [newFlower]
            , title = "Add Flower"
            , icon = Icons.plus
            , enabled = True } )
  in
  column
    [ width shrink
    , height fill
    , centerX
    , Border.rounded flowerBorderRound
    , Background.color (flowerBackgroundColor context.polarity) ]
    [ addFlowerButton ]


viewFlower : Model -> Context -> Flower -> Element Msg
viewFlower model context flower =
  case flower of
    Formula formula ->
      viewFormula model context formula
    
    Flower pistil petals ->
      let
        pistilEl =
          viewPistil model context pistil petals
        
        addPetalZone =
          case model.mode of 
            EditMode _ surgery ->
              if glueable surgery context then
                [viewAddPetalZone context pistil petals]
              else
                []
            _ ->
              []
        
        petalsEl =
          row
            [ width fill
            , height fill
            , spacing flowerBorderWidth ]
            ( Utils.List.zipperMap (viewPetal model context pistil) petals
              ++ addPetalZone )

        
        color =
          case model.mode of
            ProofMode _ -> importColor
            EditMode _ _ -> reorderColor
            _ -> Utils.Color.transparent
      in
      column
        ( [ width fill
          , height fill
          , Background.color (flowerForegroundColor context.polarity)
          , Border.color (flowerForegroundColor context.polarity)
          , Border.rounded flowerBorderRound
          , Border.shadow
              { offset = (0, 5)
              , size = 0.25
              , blur = 15
              , color = flowerForegroundColor context.polarity } ]
        ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
        ++ dragAction color model.dragDrop context.zipper flower )
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
                    droppable importColor

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

        EditMode Reordering _ ->
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
                          droppable reorderColor

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
      [ Border.width (droppable Utils.Color.transparent).borderWidth
      , Border.color Style.transparent ]

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
        
        addFlowerZone =
          case model.mode of 
            EditMode _ surgery ->
              if growable surgery context then
                [viewAddFlowerZone context bouquet]
              else
                []
            _ ->
              []
      in
      wrappedRow attrs (els ++ addFlowerZone)
        
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
    EditMode _ _ ->
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
    
    EditMode _ _ ->
      bouquetEl ()

    _ ->
      workingOnIt