module View.Manual exposing (..)

import View.Style exposing (..)
import View.Goal exposing (..)
import Model.Goal exposing (..)
import Model.App exposing (..)
import Update.App exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font

import FeatherIcons as Icons


mkIcon : Float -> Icons.Icon -> Element msg
mkIcon size icon  =
  icon |>
  Icons.withSize size |>
  Icons.toHtml [] |>
  html |>
  el []


headingIconSize : Float
headingIconSize =
  40


flowerIcon : Element msg
flowerIcon =
  el [Font.size (round headingIconSize + 10)] (text "❀")

proofIcon : Element msg
proofIcon =
  mkIcon headingIconSize Icons.checkSquare


editIcon : Element msg
editIcon =
  mkIcon headingIconSize Icons.edit2


navigateIcon : Element msg
navigateIcon =
  mkIcon headingIconSize Icons.navigation


resetIcon : Element msg
resetIcon =
  mkIcon 20 Icons.rotateCw


resetButton : SandboxID -> Element Msg
resetButton id =
  Input.button
    [ ]
    { onPress = Just (ResetSandbox id)
    , label = resetIcon
    }

viewSandbox : FlowerDnD -> Sandbox -> String -> Element Msg
viewSandbox dnd { currentGoal } id =
  row
    [ width fill
    , spacing 20
    , padding 10
    ]
    [ viewGoal dnd currentGoal
    , resetButton id
    ]
    

backButtonStyle : List (Attribute msg)
backButtonStyle =
  [ Border.width 2
  , Border.rounded 4
  , Border.solid
  , padding 10 ]


navbar : Element msg
navbar =
  row
    [ width fill
    , padding 20 ]
    [ link
        ( alignLeft :: backButtonStyle )
        { url = "/", label = text "back to app" } ]


body : Model -> Element Msg
body { manualExamples, dragDrop } =
  let
    sandbox id =
      viewSandbox dragDrop (getSandbox id manualExamples) id
    
    padder =
      el [width shrink, height fill] none
    
    (tcol, par) =
      (textColumn [spacing 15], paragraph [])
    
    (t, b, i) =
      (text, bold, italic)
    

    h1 icon txt =
      row [spacing 20, paddingXY 0 20]
        [ icon
        , el [Font.size 40] (text txt)
        ]

    h2 txt =
      row [spacing 15]
        [ el [Font.size 35] (text "•")
        , el [Font.size 25] (bold txt)
        ]
  in
  row
    ( scrollbarY ::
      fillXY )
    [ padder
    , column
      [ width (fill |> maximum 600)
      , height fill
      , padding 20
      , spacing 20
      , centerX
      ]
      [ h1 flowerIcon "Flowers"
      , par [ t"Look at this box:" ]
      , sandbox "Flower"
      , tcol
          [ par [ t"It is called a ", b"flower", t"." ]
          , par [ t"a, b, c and d are ", b"atoms", t"." ]
          , par [ t"a and b are in the ", b"pistil", t" (upper part of the box)." ]
          , par [ t"c and d are each in a ", b"petal", t"." ]
          , par [ t"Petals form the ", b"corolla", t" (lower part of the box)." ]
          , par [ t"Flowers represent ", b"logical statements", t":" ]
          , textColumn [paddingXY 30 5, spacing 5]
              [ par [ i"If a ", b"and", i" b are true," ]
              , par [ b"then", i" either c ", b"or", i" d is true." ] ]
          , par [ t"Juxtaposition is interpreted as ", b"conjunction", t"." ]
          , par [ t"A pistil ", b"implies", t" the ", b"disjunction", t" of its petals." ]
          , par [ t"Flowers can be ", b"nested", t" inside each other." ]
          , par [ t"Flowers can be ", b"proved", t", ", b"edited",  t" and ", b"navigated", t"." ]
          ]

      , h1 proofIcon "Proof Mode"

      , h2 "QED"
      , par [ t"Click on an ", el greenActionable.active (text "empty petal"), t" to erase its flower." ]
      , sandbox "QED"
      
      , h2 "Justify"
      , par [ t"Click on an ", el greenActionable.active (text "atom"), t" to erase it." ]
      , sandbox "Justify"

      , h1 editIcon "Edit Mode"

      , h1 navigateIcon "Navigation Mode"
      , par [ t"Coming soon!" ]
      ]
    , padder
    ]


page : Model -> Element Msg
page model =
  column
    [ width fill
    , height fill ]
    [ body model
    , navbar ]