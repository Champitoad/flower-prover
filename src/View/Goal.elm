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


cropAction : Context -> Flower -> List (Attribute Msg)
cropAction context flower =
  let actionableStyle = redActionable in
  if croppable context || isGrownFlower flower then
    Utils.Events.onClick (Action Crop context.zipper [flower])
    :: (htmlAttribute <| title "Remove Flower")
    :: actionableStyle.active
  else
    actionableStyle.inactive


pullAction : Context -> Garden -> List (Attribute Msg)
pullAction context garden =
  let actionableStyle = redActionable in
  if pullable context then
    Utils.Events.onClick (Action Pull context.zipper garden.flowers)
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
        
        EditMode Operating _ ->
          cropAction context (Formula formula)

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


viewPistil : Model -> Context -> Metadata -> Garden -> List Garden -> Element Msg
viewPistil model context metadata pistil petals =
  let
    newZipper =
      mkPistil metadata pistil.metadata petals :: context.zipper

    clickAction =
      case model.mode of
        ProofMode Justifying ->
          let
            actionableStyle = orangeActionable

            action rule name =
              (Events.onClick (Action rule newZipper (harvest pistil)))
              :: (htmlAttribute <| title name)
              :: actionableStyle.active
          in
          if List.isEmpty (harvest pistil) then
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
        
        EditMode Operating _ ->
          cropAction context (mkFlower metadata pistil petals)

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


viewPetal : Model -> Context -> Metadata -> Garden -> (List Garden, List Garden) -> Garden -> Element Msg
viewPetal model context metadata pistil (left, right) petal =
  let
    newZipper =
      mkPetal metadata petal.metadata pistil left right :: context.zipper

    clickAction =
      let actionableStyle = greenActionable in
      case model.mode of
        ProofMode Justifying ->
          if List.isEmpty petal.flowers then
            (Events.onClick (Action Close newZipper petal.flowers))
            :: (htmlAttribute <| title "QED")
            :: actionableStyle.active
          else
            actionableStyle.inactive
        
        EditMode Operating _ ->
          pullAction { context | zipper = newZipper } petal

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


viewAddPetalZone : Context -> Metadata -> Garden -> List Garden -> Element Msg
viewAddPetalZone context metadata pistil petals =
  let
    newFlower =
      mkFlower metadata pistil (petals ++ [mkFakeGarden []])

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
    , Border.rounded flowerBorderRound
    , Background.color (flowerBackgroundColor (invert context.polarity)) ]
    [ addPetalButton ]


viewAddFlowerZone : Context -> Bouquet -> Element Msg
viewAddFlowerZone context bouquet =
  let
    newFlower =
      mkFakeFlower
        (mkFakeGarden [])
        [mkFakeGarden []]

    newZipper =
      mkBouquet bouquet [] :: context.zipper

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
    
    Flower { metadata, pistil, petals } ->
      let
        pistilEl =
          viewPistil model context metadata pistil petals
        
        addPetalZone =
          case model.mode of 
            EditMode _ _ ->
              if glueable context then
                [viewAddPetalZone context metadata pistil petals]
              else
                []
            _ ->
              []
        
        petalsEl =
          row
            [ width fill
            , height fill
            , spacing flowerBorderWidth ]
            ( Utils.List.zipperMap (viewPetal model context metadata pistil) petals
              ++ addPetalZone )

        
        color =
          case model.mode of
            ProofMode _ -> importColor
            EditMode _ _ -> reorderColor
            _ -> Utils.Color.transparent
        

        borderColor =
          if metadata.grown then
            Color.rgb255 58 134 255
            |> Utils.Color.toElement
          else
            flowerForegroundColor context.polarity
      in
      column
        ( [ width fill
          , height fill
          , Background.color (flowerForegroundColor context.polarity) ]
         ++ (List.map htmlAttribute <| DnD.droppable DragDropMsg Nothing)
         ++ dragAction color model.dragDrop context.zipper flower
         ++
          [ Border.color borderColor
          , Border.shadow
              { offset = (0, 5)
              , size = 0.25
              , blur = 15
              , color = borderColor } ] )
        [ pistilEl, petalsEl ]


viewGarden : Model -> Context -> Garden -> Element Msg
viewGarden model context garden =
  let
    flowerEl (left, right) =
      viewFlower
        model
        { context
        | zipper = mkBouquet left right :: context.zipper }
    
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
                        if mkBouquet left right :: context.zipper == target
                        then dropStyle.active
                        else dropStyle.inactive
                    
                      _ ->
                        dropStyle.inactive
                in
                dropTargetStyle ++
                ( List.map htmlAttribute <|
                  DnD.droppable DragDropMsg
                    (Just { target = mkBouquet left right :: context.zipper
                          , content = [] }) )
              else
                []
            _ ->
              []

        EditMode Reordering _ ->
          case DnD.getDragId model.dragDrop of
            Just { source, content } ->
              case source of
                Bouquet sourceBouquet :: sourceParent ->
                  if sourceParent == context.zipper then
                    let
                      (sourceIndex, index) =
                        (List.length sourceBouquet.left, List.length left)
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
                            sourceBouquet.left ++ middle ++ content :: right
                          else
                            let
                              middle =
                                Utils.List.slice index (sourceIndex - 1) whole
                            in
                            left ++ content :: (middle ++ sourceBouquet.right)

                        dropStyle =
                          droppable reorderColor

                        dropTargetStyle =
                          case DnD.getDropId model.dragDrop of
                            Just (Just { target }) ->
                              if mkBouquet left right :: context.zipper == target
                              then dropStyle.active
                              else dropStyle.inactive

                            _ ->
                              dropStyle.inactive
                      in
                      dropTargetStyle ++
                      ( List.map htmlAttribute <|
                        DnD.droppable DragDropMsg
                          (Just { target = mkBouquet left right :: context.zipper
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
        Flower _ -> fill

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
          Utils.List.zipperMap sperse garden.flowers
        
        addFlowerZone =
          case model.mode of 
            EditMode _ _ ->
              if growable context then
                [viewAddFlowerZone context garden.flowers]
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
          dropAction (garden.flowers, [])
        
        sperse lr flower =
          el
            [ width (length flower)
            , height fill
            , centerX, centerY ]
            ( flowerEl lr flower )

        els =
          Utils.List.zipperMap sperse garden.flowers
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
              ( viewFlower model (Context [mkBouquet l r] Pos) flower ) )
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