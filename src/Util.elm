module Util exposing (..)

import Dict exposing (Dict)
import HexGrid exposing (HexGrid(..), Point)
import Html exposing (Html)
import Html.Attributes as Hattr
import Html.Events as Hevent
import Json.Decode
import Json.Encode
import State exposing (State(..))


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


traverseStateDict :
    (v -> State state v2)
    -> Dict comparable v
    -> State state (Dict comparable v2)
traverseStateDict f dict =
    let
        g :
            comparable
            -> v
            -> ( Dict comparable v2, state )
            -> ( Dict comparable v2, state )
        g k v ( accDict, state ) =
            State.run state <|
                State.map (\v2 -> Dict.insert k v2 accDict)
                    (f v)
    in
    State <| \state -> Dict.foldr g ( Dict.empty, state ) dict
