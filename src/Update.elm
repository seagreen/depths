module Update exposing (..)

-- Core

import Dict exposing (Dict)
import Random


-- 3rd

import Either exposing (Either(..))
import HexGrid exposing (HexGrid(..), Point)


-- Local

import Game exposing (Commands(..))
import Game.Id as Id exposing (Id(..), IdSeed(..))
import Game.State as Game
    exposing
        ( Buildable(..)
        , Game
        , Geology(..)
        , Habitat
        , HabitatEditor(..)
        , HabitatName
        , Tile
        , Turn(..)
        )
import Model
    exposing
        ( Msg(..)
        , Model
        , Selection(..)
        )
import Util


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SetRandomSeed (Model.NewSeed new) ->
            let
                oldGame =
                    model.game
            in
                { model | game = { oldGame | randomSeed = Random.initialSeed new } }

        EndTurn ->
            endTurn model

        HoverPoint point ->
            { model | hoverPoint = Just point }

        EndHover ->
            { model | hoverPoint = Nothing }

        SelectPoint point ->
            { model | selection = newSelection model point }

        SelectUnit id ->
            { model | selection = Just (SelectedId id) }

        SelectTile point ->
            { model | selection = Just (SelectedPoint point) }

        PlanMoves id points ->
            { model | plannedMoves = Dict.insert (Id.unId id) points model.plannedMoves }

        CancelMove id ->
            { model | plannedMoves = Dict.remove (Id.unId id) model.plannedMoves }

        BuildOrder mBuilding ->
            buildOrder model mBuilding

        NameEditorFull new ->
            setHabitatName
                (\name ->
                    case name of
                        Right _ ->
                            name

                        Left (HabitatEditor editor) ->
                            Left (HabitatEditor { editor | full = new })
                )
                model

        NameEditorAbbreviation new ->
            setHabitatName
                (\name ->
                    case name of
                        Right _ ->
                            name

                        Left (HabitatEditor editor) ->
                            Left (HabitatEditor { editor | abbreviation = new })
                )
                model

        NameEditorSubmit ->
            setHabitatName
                (\name ->
                    case name of
                        Right _ ->
                            name

                        Left (HabitatEditor editor) ->
                            Right editor
                )
                model


endTurn : Model -> Model
endTurn model =
    let
        ( immediateMoves, laterMoves ) =
            splitPlannedMoves model.plannedMoves

        ( reports, newGameState ) =
            Game.resolveTurn immediateMoves model.game
    in
        { model
            | game = newGameState
            , plannedMoves = cleanPlannedMoves newGameState laterMoves
            , selection = updateSelection newGameState model.selection
            , gameLog = reports ++ model.gameLog
        }


{-| Remove plans to move units that are no longer on the board.
-}
cleanPlannedMoves : Game -> Dict Int (List Point) -> Dict Int (List Point)
cleanPlannedMoves game moveDict =
    let
        friendlies =
            Game.friendlyUnitDict (Util.unHexGrid game.grid)

        go id movePoint acc =
            if Dict.member id friendlies then
                Dict.insert id movePoint acc
            else
                acc
    in
        Dict.foldr go Dict.empty moveDict


{-| Split planned moves into those to be executed this turn and those for later.
-}
splitPlannedMoves : Dict Int (List Point) -> ( Commands, Dict Int (List Point) )
splitPlannedMoves moves =
    let
        go :
            Int
            -> List Point
            -> ( Commands, Dict Int (List Point) )
            -> ( Commands, Dict Int (List Point) )
        go id moves ( Commands immediate, forLater ) =
            case moves of
                [] ->
                    ( Commands immediate, forLater )

                x :: xs ->
                    ( Commands (Dict.insert id x immediate)
                    , case xs of
                        [] ->
                            forLater

                        _ ->
                            Dict.insert id xs forLater
                    )
    in
        Dict.foldr go ( Commands Dict.empty, Dict.empty ) moves


{-| Update the selection after the end of a turn.
-}
updateSelection : Game -> Maybe Selection -> Maybe Selection
updateSelection game oldSelection =
    let
        stillActive id =
            Game.findUnit id (Util.unHexGrid game.grid)

        maybeBecameHabitat id =
            Game.habitatDict game.grid
                |> Dict.toList
                |> (\habList ->
                        case List.filter (\( _, hab ) -> hab.createdBy == id) habList of
                            [ ( point, _ ) ] ->
                                Just (SelectedPoint point)

                            _ ->
                                Nothing
                   )
    in
        oldSelection
            |> Maybe.andThen
                (\selection ->
                    case selection of
                        SelectedPoint _ ->
                            oldSelection

                        SelectedId id ->
                            case stillActive id of
                                Nothing ->
                                    maybeBecameHabitat id

                                Just _ ->
                                    oldSelection
                )


{-| Update the selection after a user clicks on the grid.

This is a little complex, since (eg) if the click is on empty water
we select the water tile itself, but if the click is on water and it
contains a single naval unit we go ahead and select that unit by id.
-}
newSelection : Model -> Point -> Maybe Selection
newSelection model newPoint =
    let
        newPointOrId : Maybe Selection
        newPointOrId =
            HexGrid.valueAt newPoint model.game.grid
                |> Maybe.andThen
                    (\tile ->
                        case tile.fixed of
                            Mountain (Just _) ->
                                Just (SelectedPoint newPoint)

                            _ ->
                                case List.head (Game.friendlyUnits tile) of
                                    Nothing ->
                                        Just (SelectedPoint newPoint)

                                    Just sub ->
                                        Just (SelectedId sub.id)
                    )
    in
        case model.selection of
            Nothing ->
                newPointOrId

            Just selection ->
                case selection of
                    SelectedId _ ->
                        newPointOrId

                    SelectedPoint oldPoint ->
                        -- If the user clicked on a tile that's currently selected,
                        -- unselect it.
                        if newPoint == oldPoint then
                            Nothing
                        else
                            newPointOrId


setHabitatName :
    (Either HabitatEditor HabitatName -> Either HabitatEditor HabitatName)
    -> Model
    -> Model
setHabitatName updateName model =
    let
        updatePoint tile =
            case tile.fixed of
                Mountain (Just hab) ->
                    let
                        newFixed =
                            Mountain (Just { hab | name = updateName hab.name })
                    in
                        { tile | fixed = newFixed }

                _ ->
                    tile

        oldGame =
            model.game
    in
        case Model.focusPoint model of
            Just point ->
                { model
                    | game =
                        { oldGame | grid = HexGrid.update point updatePoint model.game.grid }
                }

            _ ->
                model


buildOrder : Model -> Maybe Buildable -> Model
buildOrder model mBuildable =
    let
        setProduction tile =
            case tile.fixed of
                Mountain (Just hab) ->
                    { tile
                        | fixed =
                            Mountain
                                (Just
                                    { hab
                                        | producing = mBuildable
                                        , produced = 0
                                    }
                                )
                    }

                _ ->
                    tile

        oldGame =
            model.game
    in
        { model
            | game =
                { oldGame
                    | grid =
                        case Model.focusPoint model of
                            Nothing ->
                                oldGame.grid

                            Just point ->
                                HexGrid.update point setProduction oldGame.grid
                }
        }
