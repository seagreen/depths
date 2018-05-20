module Game.Type.Tile exposing (..)

import Dict exposing (Dict)
import Game.Type.Geology exposing (Geology(..))
import Game.Type.Habitat exposing (Habitat)
import Game.Type.Unit exposing (Unit)


{-| Dict keys are the subs' Ids.
-}
type alias Tile =
    { units : Dict Int Unit
    , fixed : Geology
    }


emptyMountain : Tile
emptyMountain =
    Tile Dict.empty (Mountain Nothing)
