module Update exposing (..)

-- Core

import Dict exposing (Dict)
import Random


-- 3rd

import Either exposing (Either(..))
import HexGrid exposing (HexGrid(..), Point)


-- Local

import Model
    exposing
        ( Msg(..)
        , Model
        , Habitat
        , HabitatName
        , HabitatEditor(..)
        , Buildable(..)
        , Tile
        , Geology(..)
        , Selection(..)
        )
import ResolveTurn
import Id exposing (Id(..), IdSeed(..))


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SetRandomSeed (Model.NewSeed new) ->
            { model | randomSeed = Random.initialSeed new }

        EndTurn ->
            ResolveTurn.resolveTurn model

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

        PlanMove selected id new ->
            planMove model selected id new

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
            HexGrid.valueAt newPoint model.grid
                |> Maybe.andThen
                    (\tile ->
                        case tile.fixed of
                            Mountain _ ->
                                Just (SelectedPoint newPoint)

                            _ ->
                                case List.head (Model.friendlyUnits tile) of
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
    in
        case Model.focusPoint model of
            Just point ->
                { model | grid = HexGrid.update point updatePoint model.grid }

            _ ->
                model


planMove : Model -> Point -> Id -> Point -> Model
planMove model selected id new =
    let
        (HexGrid _ grid) =
            model.grid

        updateUnits =
            Dict.update
                (Id.unId id)
                (Maybe.andThen (\sub -> Just { sub | plannedMove = Just new }))

        newGrid =
            HexGrid.update selected
                (\tile ->
                    { tile | units = updateUnits tile.units }
                )
                model.grid
    in
        { model | grid = newGrid }


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
    in
        { model
            | grid =
                case Model.focusPoint model of
                    Nothing ->
                        model.grid

                    Just point ->
                        HexGrid.update point setProduction model.grid
        }
