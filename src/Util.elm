module Util exposing (..)

import Dict exposing (Dict)
import HexGrid exposing (HexGrid(..), Point)
import Html exposing (Html)
import Html.Attributes as Hattr
import Html.Events as Hevent
import Json.Decode
import Json.Encode


badge : List (Html msg) -> Html msg
badge =
    Html.span [ Hattr.class "badge badge-secondary" ]


{-| <https://github.com/elm-lang/html/issues/136>
-}
label_ : String -> Html.Attribute msg
label_ s =
    Hattr.property "label" (Json.Encode.string s)


{-| Generic onchange handler for <select>
-}
onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    Hevent.on "change" <|
        Json.Decode.map handler <|
            Json.Decode.at [ "target", "value" ] Json.Decode.string


unHexGrid : HexGrid a -> Dict Point a
unHexGrid (HexGrid _ grid) =
    grid
