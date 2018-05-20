module Game.Type.Unit exposing (..)

import Game.Type.Building exposing (Building(..))
import Game.Type.Id exposing (Id(..))
import Html exposing (Html)


type Player
    = Player1
    | Player2


playerToString : Player -> String
playerToString player =
    case player of
        Player1 ->
            "Player 1"

        Player2 ->
            "Player 2"


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
    = ColonySub
    | DieselSub
    | AttackSub
    | Boomer


all : List Submarine
all =
    [ ColonySub
    , DieselSub
    , AttackSub
    , Boomer
    ]


stats : Submarine -> Stats
stats sub =
    case sub of
        ColonySub ->
            { name = "Colony Submarine"
            , abbreviation = "CS"
            , cost = 15
            , prerequisites = [ Dock, Dormitory ]
            , speed = 2
            , sensors = 2
            , stealth = 0
            , firepower = 0
            }

        DieselSub ->
            { name = "Diesel Submarine"
            , abbreviation = "DS"
            , cost = 12
            , prerequisites = [ Dock ]
            , speed = 1
            , sensors = 2
            , stealth = 0
            , firepower = 3
            }

        AttackSub ->
            { name = "Attack Submarine"
            , abbreviation = "SSN"
            , cost = 20
            , prerequisites = [ Dock, Reactor ]
            , speed = 4
            , sensors = 4
            , stealth = 3
            , firepower = 4
            }

        Boomer ->
            { name = "Boomer"
            , abbreviation = "B"
            , cost = 20
            , prerequisites = [ Dock, Reactor ]
            , speed = 2
            , sensors = 6
            , stealth = 0
            , firepower = 6
            }


fromString : String -> Maybe Submarine
fromString s =
    case s of
        "ColonySub" ->
            Just ColonySub

        "DieselSub" ->
            Just DieselSub

        "AttackSub" ->
            Just AttackSub

        "Boomer" ->
            Just Boomer

        _ ->
            Nothing


helpText : Submarine -> Maybe (Html a)
helpText sub =
    case sub of
        ColonySub ->
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
