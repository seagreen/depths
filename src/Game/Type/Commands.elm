module Game.Type.Commands exposing (..)

import Dict exposing (Dict)
import Game.Type.Buildable exposing (Buildable(..))
import Game.Type.Habitat as Habitat
import HexGrid exposing (Point)


{-| The keys in `moves` are the IDs of units.
-}
type alias Commands =
    { moves : Dict Int Point
    , buildOrders : Dict Point Buildable
    , habitatNamings : Dict Int Habitat.Name
    }
