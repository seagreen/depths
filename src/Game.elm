module Game exposing (..)

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


type alias State =
    { grid : HexGrid Tile
    , turn : Turn
    , idSeed : IdSeed
    , randomSeed : Random.Seed
    }


init : State
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
        , turn = Turn 1
        , idSeed = idSeed
        , randomSeed =
            -- The Main module randomizes this at startup.
            Random.initialSeed 0
        }


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
