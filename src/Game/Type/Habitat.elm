module Game.Type.Habitat exposing (..)

import Game.Type.Buildable exposing (Buildable)
import Game.Type.Building as Building exposing (Building(..))
import Game.Type.Id as Id exposing (Id(..))
import Game.Type.Player exposing (Player(..))


type alias Habitat =
    { name : Maybe Name
    , player : Player

    -- Carried over from the id of the colony sub that created
    -- the habitat:
    , id : Id
    , buildings : List Building
    , producing : Maybe Buildable
    , produced : Int
    }


new : Player -> Id -> Habitat
new player colonySubId =
    { name = Nothing
    , player = player
    , id = colonySubId
    , buildings = [ PrefabHabitat ]
    , producing = Nothing
    , produced = 0
    }


type alias Name =
    { full : String
    , abbreviation : String
    }


fullNameWithDefault : Habitat -> String
fullNameWithDefault hab =
    case hab.name of
        Nothing ->
            "<New habitat>"

        Just name ->
            name.full


abbreviationWithDefault : Habitat -> String
abbreviationWithDefault hab =
    case hab.name of
        Nothing ->
            "<N>"

        Just name ->
            name.abbreviation


type NameEditor
    = NameEditor Name


unNameEditor : NameEditor -> Name
unNameEditor (NameEditor editor) =
    editor


emptyNameEditor : NameEditor
emptyNameEditor =
    NameEditor
        { full = ""
        , abbreviation = ""
        }
