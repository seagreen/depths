module Model exposing (..)

-- Core

import Dict exposing (Dict)
import Random


-- 3rd

import Either exposing (Either(..))
import HexGrid exposing (HexGrid(..), Direction, Point)
import State


-- Local

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
    { grid : HexGrid Tile
    , selection : Maybe Selection
    , hoverPoint : Maybe Point
    , gameLog : List BattleReport
    , turn : Turn
    , idSeed : IdSeed
    , randomSeed : Random.Seed
    }


init : Model
init =
    let
        ( ( idA, idB ), idSeed ) =
            State.run (IdSeed 1) <|
                State.map2 (,) Id.next Id.next
    in
        { grid =
            HexGrid.fromList 6
                (Tile Dict.empty Depths)
                [ ( ( -4, 1 ), emptyMountain )
                , ( ( -1, -3 ), emptyMountain )
                , ( ( 2, -4 ), emptyMountain )
                , ( ( 3, -1 ), emptyMountain )
                , ( ( 2, 2 ), emptyMountain )
                , ( ( -3, 3 ), emptyMountain )
                , ( ( -2, 4 ), emptyMountain )
                , ( ( -4, -2 )
                  , Tile
                        (Dict.singleton
                            (Id.unId idA)
                            (Unit idA Human ColonySubmarine Nothing)
                        )
                        Depths
                  )
                , ( ( 0, 0 )
                  , Tile
                        (Dict.singleton
                            (Id.unId idB)
                            (Unit idB Computer AttackSubmarine Nothing)
                        )
                        Depths
                  )
                ]
        , selection = Nothing
        , hoverPoint = Nothing
        , gameLog = []
        , turn = Turn 1
        , idSeed = idSeed
        , randomSeed =
            -- The Main module randomizes this at startup.
            Random.initialSeed 0
        }


type Outcome
    = Victory
    | Defeat


outcome : Model -> Maybe Outcome
outcome model =
    if unTurn model.turn >= 200 then
        Just Victory
    else
        let
            habitats =
                habitatDict model.grid

            (HexGrid _ dict) =
                model.grid
        in
            if Dict.size habitats >= 4 then
                Just Victory
            else if
                Dict.isEmpty (habitatDict model.grid)
                    && List.isEmpty (friendlyUnitList dict)
            then
                Just Defeat
            else
                Nothing


type Turn
    = Turn Int


unTurn : Turn -> Int
unTurn (Turn n) =
    n


{-| Dict keys are the subs' Ids.
-}
type alias Tile =
    { units : Dict Int Unit
    , fixed : Geology
    }


emptyMountain : Tile
emptyMountain =
    Tile Dict.empty (Mountain Nothing)


type Geology
    = Depths
    | Mountain (Maybe Habitat)


type alias Habitat =
    { name : Either HabitatEditor HabitatName
    , createdBy : Id
    , buildings : List Building
    , producing : Maybe Buildable
    , produced : Int
    }


productionUntilCompletion : Habitat -> Maybe Int
productionUntilCompletion hab =
    Maybe.map
        (\producing -> cost producing - hab.produced)
        hab.producing


newHabitat : Id -> Habitat
newHabitat colonySubId =
    { name = Left emptyNameEditor
    , createdBy = colonySubId
    , buildings = [ PrefabHabitat ]
    , producing = Nothing
    , produced = 0
    }


habitatFullName : Habitat -> String
habitatFullName hab =
    case hab.name of
        Left _ ->
            "<New habitat>"

        Right name ->
            name.full


habitatAbbreviation : Habitat -> String
habitatAbbreviation hab =
    case hab.name of
        Left _ ->
            "<N>"

        Right name ->
            name.abbreviation


habitatDict : HexGrid Tile -> Dict Point Habitat
habitatDict (HexGrid _ grid) =
    Dict.foldr
        (\point tile acc ->
            case tile.fixed of
                Mountain (Just hab) ->
                    Dict.insert point hab acc

                _ ->
                    acc
        )
        Dict.empty
        grid


habitatFromTile : Tile -> Maybe Habitat
habitatFromTile tile =
    case tile.fixed of
        Mountain (Just hab) ->
            Just hab

        _ ->
            Nothing


type alias HabitatName =
    { full : String
    , abbreviation : String
    }


type HabitatEditor
    = HabitatEditor HabitatName


unHabitatEditor : HabitatEditor -> HabitatName
unHabitatEditor (HabitatEditor editor) =
    editor


emptyNameEditor : HabitatEditor
emptyNameEditor =
    HabitatEditor
        { full = ""
        , abbreviation = ""
        }


type alias BattleReport =
    { turn : Turn
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
                        Maybe.map Tuple.first (findUnit id model.grid)
            )


focus : Model -> Maybe ( Point, Tile )
focus model =
    let
        (HexGrid _ dict) =
            model.grid
    in
        focusPoint model
            |> Maybe.andThen
                (\point ->
                    Maybe.map
                        (\tile -> ( point, tile ))
                        (Dict.get point dict)
                )


type Buildable
    = BuildSubmarine Submarine
    | BuildBuilding Building


name : Buildable -> String
name buildable =
    case buildable of
        BuildSubmarine sub ->
            (Unit.stats sub).name

        BuildBuilding building ->
            (Building.stats building).name


cost : Buildable -> Int
cost buildable =
    case buildable of
        BuildSubmarine sub ->
            (Unit.stats sub).cost

        BuildBuilding building ->
            (Building.stats building).cost


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
