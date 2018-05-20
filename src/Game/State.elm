module Game.State exposing (..)

import Dict exposing (Dict)
import Game.Type.Geology as Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id as Id exposing (Id(..), IdSeed(..))
import Game.Type.Player exposing (Player(..))
import Game.Type.Tile as Tile exposing (Tile)
import Game.Type.Turn exposing (Turn(..))
import Game.Type.Unit as Unit exposing (Submarine(..), Unit)
import HexGrid exposing (Direction, HexGrid(..), Point)
import Random.Pcg as Random


{-| The game's state.

I considered naming this `State`, but the turn resolution
code uses it alongside `State` from `folkertdev/elm-state`
so much that things got confusing.

-}
type alias Game =
    { grid : HexGrid Tile
    , turn : Turn

    -- Used for giving new units Ids. Incrmental.
    , nextUnitId : IdSeed

    -- Used for determining battle events.
    , randomSeed : Random.Seed
    }


init : Game
init =
    { grid =
        HexGrid.fromList 6
            (Tile Dict.empty Depths)
            [ ( ( -4, 1 ), Tile.emptyMountain )
            , ( ( -1, -3 ), Tile.emptyMountain )
            , ( ( 2, -4 ), Tile.emptyMountain )
            , ( ( 3, -1 ), Tile.emptyMountain )
            , ( ( 2, 2 ), Tile.emptyMountain )
            , ( ( -3, 3 ), Tile.emptyMountain )
            , ( ( -2, 4 ), Tile.emptyMountain )
            , ( ( -4, -2 )
              , Tile
                    (Dict.singleton
                        1
                        (Unit (Id 1) Player1 ColonySub)
                    )
                    Depths
              )
            , ( ( 6, 0 )
              , Tile
                    (Dict.singleton
                        2
                        (Unit (Id 2) Player2 ColonySub)
                    )
                    Depths
              )
            ]
    , turn = Turn 1
    , nextUnitId = IdSeed 3
    , randomSeed = Random.initialSeed 0
    }


initDebug : Game
initDebug =
    { grid =
        HexGrid.fromList 6
            (Tile Dict.empty Depths)
            [ ( ( -4, 1 )
              , Tile Dict.empty (Mountain (Just <| Habitat.new Player1 (Id 1)))
              )
            , ( ( -1, -3 )
              , Tile Dict.empty (Mountain (Just <| Habitat.new Player2 (Id 2)))
              )
            , ( ( 2, -4 ), Tile.emptyMountain )
            , ( ( 3, -1 ), Tile.emptyMountain )
            , ( ( 2, 2 ), Tile.emptyMountain )
            , ( ( -3, 3 ), Tile.emptyMountain )
            , ( ( -2, 4 ), Tile.emptyMountain )
            , ( ( -4, -2 )
              , Tile
                    (Dict.singleton
                        1
                        (Unit (Id 1) Player1 ColonySub)
                    )
                    Depths
              )
            , ( ( 6, 0 )
              , Tile
                    (Dict.singleton
                        2
                        (Unit (Id 2) Player2 ColonySub)
                    )
                    Depths
              )
            ]
    , turn = Turn 1
    , nextUnitId = IdSeed 3
    , randomSeed = Random.initialSeed 0
    }


habitatsForPlayer : Player -> Game -> List Habitat
habitatsForPlayer player game =
    let
        (HexGrid _ grid) =
            game.grid
    in
    habitatDict grid
        |> Dict.values
        |> List.filter (\hab -> hab.player == player)


habitatDict : Dict Point Tile -> Dict Point Habitat
habitatDict grid =
    let
        f : Point -> Tile -> Dict Point Habitat -> Dict Point Habitat
        f point tile acc =
            case tile.fixed of
                Mountain (Just hab) ->
                    Dict.insert point hab acc

                _ ->
                    acc
    in
    Dict.foldr f
        Dict.empty
        grid


habitatFromTile : Tile -> Maybe Habitat
habitatFromTile tile =
    case tile.fixed of
        Mountain (Just hab) ->
            Just hab

        Mountain Nothing ->
            Nothing

        Depths ->
            Nothing


habitatFromPoint : Point -> Dict Point Tile -> Maybe Habitat
habitatFromPoint point grid =
    Dict.get point grid
        |> Maybe.andThen habitatFromTile


updateHabitat : Point -> (Habitat -> Habitat) -> Dict Point Tile -> Dict Point Tile
updateHabitat point updateHab grid =
    let
        updateHabitatAtTile : Tile -> Tile
        updateHabitatAtTile tile =
            case tile.fixed of
                Mountain (Just hab) ->
                    { tile | fixed = Mountain (Just (updateHab hab)) }

                Mountain Nothing ->
                    tile

                Depths ->
                    tile
    in
    Dict.update point (Maybe.map updateHabitatAtTile) grid


updateHabitatById : Id -> (Habitat -> Habitat) -> Dict Point Tile -> Dict Point Tile
updateHabitatById habId updateHab grid =
    let
        target : Maybe ( Point, Habitat )
        target =
            grid
                |> habitatDict
                |> Dict.toList
                |> List.filter (\( _, hab ) -> hab.id == habId)
                |> List.head
    in
    case target of
        Nothing ->
            grid

        Just ( point, _ ) ->
            updateHabitat point updateHab grid


findUnit : Id -> Dict Point Tile -> Maybe ( Point, Unit )
findUnit id grid =
    let
        f : Point -> Tile -> Maybe ( Point, Unit ) -> Maybe ( Point, Unit )
        f point tile acc =
            case Dict.get (Id.unId id) tile.units of
                Nothing ->
                    acc

                Just sub ->
                    Just ( point, sub )
    in
    Dict.foldr f Nothing grid


friendlyUnits : Player -> Tile -> List Unit
friendlyUnits currentPlayer tile =
    List.filter
        (\unit -> unit.player == currentPlayer)
        (Dict.values tile.units)


{-| Get all friendly units in the game and their locations.

Returned `Int` keys are unit IDs.

-}
unitDict : Dict Point Tile -> Dict Int Point
unitDict grid =
    unitList grid
        |> List.map (\( point, unit ) -> ( Id.unId unit.id, point ))
        |> Dict.fromList


friendlyUnitDict : Player -> Dict Point Tile -> Dict Int Point
friendlyUnitDict currentPlayer grid =
    friendlyUnitList currentPlayer grid
        |> List.map (\( point, unit ) -> ( Id.unId unit.id, point ))
        |> Dict.fromList


unitList : Dict Point Tile -> List ( Point, Unit )
unitList =
    Dict.foldr
        (\point tile acc ->
            List.map (\unit -> ( point, unit )) (Dict.values tile.units) ++ acc
        )
        []


friendlyUnitList : Player -> Dict Point Tile -> List ( Point, Unit )
friendlyUnitList currentPlayer =
    List.filterMap
        (\( point, unit ) ->
            if unit.player == currentPlayer then
                Just ( point, unit )
            else
                Nothing
        )
        << unitList
