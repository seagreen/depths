module Unit exposing (..)

-- 3rd

import HexGrid exposing (Point)


-- Local

import Building exposing (Building(..))
import Id exposing (Id(..))


type Player
    = Human
    | Computer


{-| Be aware that even if plannedMove is Just, if it's set
to the current location of the sub it's not really a move at all.
-}
type alias Unit =
    { id : Id
    , player : Player
    , class : Submarine
    , plannedMove : Maybe Point
    }


type alias Stats =
    { name : String
    , abbreviation : String
    , cost : Int
    , prerequisites : List Building
    , speed : Int
    , sensors : Int
    , stealth : Int
    , firepower :
        Int
        -- Named "firepower" instead of "attack" to clarify that it
        -- works the same on both offence and defense.
    , helpText : Maybe String
    }


type Submarine
    = ColonySubmarine
    | RemotelyOperatedVehicle
    | AttackSubmarine


all : List Submarine
all =
    [ ColonySubmarine
    , RemotelyOperatedVehicle
    , AttackSubmarine
    ]


stats : Submarine -> Stats
stats sub =
    case sub of
        ColonySubmarine ->
            { name = "Colony Submarine"
            , abbreviation = "CS"
            , cost = 20
            , prerequisites = [ SubmarinePen, Residences ]
            , speed = 2
            , sensors = 2
            , stealth = 0
            , firepower = 0
            , helpText = Just "To move click a neighboring tile then press enter. Move over an undersea mountain to create a habitat."
            }

        RemotelyOperatedVehicle ->
            { name = "Remotely Operated Vehicle"
            , abbreviation = "ROV"
            , cost = 10
            , prerequisites = [ SubmarinePen ]
            , speed = 2
            , sensors = 2
            , stealth = 0
            , firepower = 2
            , helpText = Nothing
            }

        AttackSubmarine ->
            { name = "Attack Submarine"
            , abbreviation = "SSN"
            , cost = 30
            , prerequisites = [ SubmarinePen, Armory, Dormitory ]
            , speed = 4
            , sensors = 4
            , stealth = 3
            , firepower = 3
            , helpText = Nothing
            }


fromString : String -> Maybe Submarine
fromString s =
    case s of
        "ColonySubmarine" ->
            Just ColonySubmarine

        "RemotelyOperatedVehicle" ->
            Just RemotelyOperatedVehicle

        "AttackSubmarine" ->
            Just AttackSubmarine

        _ ->
            Nothing


hasPrerequisites : Submarine -> List Building -> Bool
hasPrerequisites proposedSubmarine currentBuildings =
    List.all
        (\prereq -> List.member prereq currentBuildings)
        (stats proposedSubmarine).prerequisites


buildable : List Building -> List Submarine
buildable currentBuildings =
    -- I really wish Buildings were comparable so we could use sets.
    List.filter
        (\sub -> hasPrerequisites sub currentBuildings)
        all
