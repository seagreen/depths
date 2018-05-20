module Game.Type.Habitat exposing (..)

import Either exposing (Either(..))
import Game.Building as Building exposing (Building(..))
import Game.Id as Id exposing (Id(..))
import Game.Type.Buildable exposing (Buildable)
import Game.Unit as Unit exposing (Player(..), Submarine(..), Unit)


type alias Habitat =
    { name : Either NameEditor Name
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
    { name = Left emptyNameEditor
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
        Left _ ->
            "<New habitat>"

        Right name ->
            name.full


abbreviationWithDefault : Habitat -> String
abbreviationWithDefault hab =
    case hab.name of
        Left _ ->
            "<N>"

        Right name ->
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
