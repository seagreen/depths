module Game.Unit exposing (..)

import Game.Building exposing (Building(..))
import Game.Id exposing (Id(..))
import Html exposing (Html)


type Player
    = Player1
    | Player2


type alias Unit =
    { id : Id
    , player : Player
    , class : Submarine
    }


type alias Stats =
    { name : String
    , abbreviation : String
    , cost : Int
    , prerequisites : List Building
    , speed : Int
    , sensors : Int
    , stealth : Int

    -- Named "firepower" instead of "attack" to clarify that it
    -- works the same on both offence and defense.
    , firepower :
        Int
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


helpText : Submarine -> Maybe (Html a)
helpText sub =
    case sub of
        ColonySubmarine ->
            Just <|
                Html.div
                    []
                    [ Html.p
                        []
                        [ Html.text "To move right-click a tile and then press enter until the unit reaches it." ]
                    , Html.p
                        []
                        [ Html.text "Move over an undersea mountain (a gray tile) to create a habitat." ]
                    ]

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
