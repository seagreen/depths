module Util exposing (..)

-- Core

import Dict exposing (Dict)


-- 3rd

import HexGrid exposing (HexGrid(..), Point)
import State exposing (State(..))


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
