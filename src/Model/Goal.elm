module Model.Goal exposing (..)

import Model.Flower exposing (..)


-- A goal is a bouquet, the focus, together with a context


type alias Goal
  = { focus : Bouquet
    , context : Context }


fromBouquet : Bouquet -> Goal
fromBouquet bouquet =
  { focus = bouquet, context = Context [] Pos }


map : (Bouquet -> Bouquet) -> Goal -> Goal
map f goal =
  { goal | focus = f goal.focus }