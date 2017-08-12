module Model exposing (..)

-- Core

import Dict exposing (Dict)
import Random


-- 3rd

import HexGrid exposing (HexGrid(..), Direction, Point)


-- Local

import Game exposing (Buildable(..), Tile)
import Game.Building as Building exposing (Building(..))
import Game.Unit as Unit exposing (Unit, Player(..), Submarine(..))
import Game.Id as Id exposing (Id(..), IdSeed(..))


type Msg
    = NoOp
    | SetRandomSeed NewSeed
    | EndTurn
      -- When a point is click on the board.
      --
      -- This is more complicated than SelectUnit
      -- or SelectPoint (which are for clicking the help
      -- boxes for subs or cities respectively) since it
      -- can also do things like unselect the point
      -- if it's already selected.
    | SelectPoint Point
    | SelectUnit Id
    | SelectTile Point
    | HoverPoint Point
    | EndHover
    | PlanMove Point Id Point
    | BuildOrder (Maybe Buildable)
    | NameEditorFull String
    | NameEditorAbbreviation String
    | NameEditorSubmit


type alias Model =
    { game : Game.State
    , selection : Maybe Selection
    , hoverPoint : Maybe Point
    , gameLog : List BattleReport
    }


init : Model
init =
    { game = Game.init
    , selection = Nothing
    , hoverPoint = Nothing
    , gameLog = []
    }


type Outcome
    = Victory
    | Defeat


outcome : Model -> Maybe Outcome
outcome model =
    if Game.unTurn model.game.turn >= 200 then
        Just Victory
    else
        let
            habitats =
                Game.habitatDict model.game.grid

            (HexGrid _ dict) =
                model.game.grid
        in
            if Dict.size habitats >= 4 then
                Just Victory
            else if
                Dict.isEmpty habitats
                    && List.isEmpty (friendlyUnitList dict)
            then
                Just Defeat
            else
                Nothing


type alias BattleReport =
    { turn : Game.Turn
    , habitat : String
    , events : List BattleEvent
    }


type
    BattleEvent
    -- When the human player detects a computer unit or building.
    -- The first argument is the thing detected, the second is the
    -- detector.
    = DetectionEvent Buildable Buildable
      -- When either the human player or the computer destorys an enemy.
      -- The `Player` is the owner of the destroyed unit.
      -- The first unit is the one that was destroyed, the second
      -- (if known) is the destroyer.
    | DestructionEvent Player Buildable (Maybe Buildable)


type Selection
    = SelectedPoint Point
    | SelectedId Id


{-| "Focused" how we'll refer to a tile that's
either selected or the contains a selected submarine.
-}
focusPoint : Model -> Maybe Point
focusPoint model =
    model.selection
        |> Maybe.andThen
            (\selection ->
                case selection of
                    SelectedPoint point ->
                        Just point

                    SelectedId id ->
                        Maybe.map Tuple.first (findUnit id model.game.grid)
            )


focus : Model -> Maybe ( Point, Tile )
focus model =
    let
        (HexGrid _ dict) =
            model.game.grid
    in
        focusPoint model
            |> Maybe.andThen
                (\point ->
                    Maybe.map
                        (\tile -> ( point, tile ))
                        (Dict.get point dict)
                )


type Combatant
    = CMUnit Unit
    | CMBuilding Building


buildableFromCombatant : Combatant -> Buildable
buildableFromCombatant combatant =
    case combatant of
        CMUnit unit ->
            BuildSubmarine unit.class

        CMBuilding building ->
            BuildBuilding building


combatantSensors : Combatant -> Int
combatantSensors combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).sensors

        CMBuilding building ->
            Building.sensors building


combatantStealth : Combatant -> Int
combatantStealth combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).stealth

        CMBuilding _ ->
            0


combatantFirepower : Combatant -> Int
combatantFirepower combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).firepower

        CMBuilding building ->
            Building.firepower building


findUnit : Id -> HexGrid Tile -> Maybe ( Point, Unit )
findUnit id (HexGrid _ grid) =
    let
        f point tile acc =
            case Dict.get (Id.unId id) tile.units of
                Nothing ->
                    acc

                Just sub ->
                    Just ( point, sub )
    in
        Dict.foldr f Nothing grid


friendlyUnits : Tile -> List Unit
friendlyUnits tile =
    List.filter
        (\unit -> unit.player == Human)
        (Dict.values tile.units)


unitDict : Dict Point Tile -> Dict Point (Dict Int Unit)
unitDict =
    Dict.map
        (\_ tile ->
            tile.units
        )


unitList : Dict Point Tile -> List ( Point, Unit )
unitList =
    Dict.foldr
        (\point tile acc ->
            List.map (\unit -> ( point, unit )) (Dict.values tile.units) ++ acc
        )
        []


friendlyUnitList : Dict Point Tile -> List ( Point, Unit )
friendlyUnitList =
    List.filterMap
        (\( point, unit ) ->
            case unit.player of
                Human ->
                    Just ( point, unit )

                Computer ->
                    Nothing
        )
        << unitList


type NewSeed
    = NewSeed Int


newRandomSeed : Cmd Msg
newRandomSeed =
    Random.generate
        (SetRandomSeed << NewSeed)
        -- Using 999999 instead of Random.maxInt here for more human-sized values
        -- (to make them more readable in the debugger and that kind of thing).
        (Random.int 0 999999)
