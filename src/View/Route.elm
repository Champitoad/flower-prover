module View.Route exposing (..)

import Url
import Url.Parser exposing (Parser, map, oneOf, s, top, parse, (</>))

type Route
 = App
 | Manual
 | NotFound String
 
routeParser : Parser (Route -> a) a
routeParser =
  let
    lix = s "Labo" </> s "Pablo.DONATO" </> s "flowerprover"
  in
  oneOf
    [ map App top
    , map Manual (s "manual")

    , map App lix
    , map Manual (lix </> s "manual")
    ]

fromUrl : Url.Url -> Route
fromUrl url =
  Maybe.withDefault (NotFound url.path) (parse routeParser url)