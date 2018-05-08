module Update exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Game exposing (Commands)
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
import Game.Unit exposing (Player(..))
import HexGrid exposing (HexGrid(..), Point)
import Json.Decode as Decode
import Model
    exposing
        ( GameType(..)
        , Model
        , Msg(..)
        , Selection(..)
        )
import Protocol exposing (Message(..), NetworkMessage)
import Random
import Util


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        EndRound ->
            endRoundOnlineGame model

        HoverPoint point ->
            ( { model | hoverPoint = Just point }
            , Cmd.none
            )

        EndHover ->
            ( { model | hoverPoint = Nothing }, Cmd.none )

        SelectPoint point ->
            ( { model | selection = newSelection model point }, Cmd.none )

        SelectUnit id ->
            ( { model | selection = Just (SelectedId id) }, Cmd.none )

        SelectTile point ->
            ( { model | selection = Just (SelectedPoint point) }, Cmd.none )

        PlanMoves id points ->
            ( { model | plannedMoves = Dict.insert (Id.unId id) points model.plannedMoves }, Cmd.none )

        CancelMove id ->
            ( { model | plannedMoves = Dict.remove (Id.unId id) model.plannedMoves }, Cmd.none )

        StopBuilding ->
            ( stopBuilding model, Cmd.none )

        BuildOrder buildable ->
            ( buildOrder model buildable, Cmd.none )

        NameEditorFull new ->
            ( setHabitatName
                (\name ->
                    case name of
                        Right _ ->
                            name

                        Left (HabitatEditor editor) ->
                            Left (HabitatEditor { editor | full = new })
                )
                model
            , Cmd.none
            )

        NameEditorAbbreviation new ->
            ( setHabitatName
                (\name ->
                    case name of
                        Right _ ->
                            name

                        Left (HabitatEditor editor) ->
                            Left (HabitatEditor { editor | abbreviation = new })
                )
                model
            , Cmd.none
            )

        NameEditorSubmit ->
            ( setHabitatName
                (\name ->
                    case name of
                        Right _ ->
                            name

                        Left (HabitatEditor editor) ->
                            Right editor
                )
                model
            , Cmd.none
            )

        SetServerUrl url ->
            let
                server =
                    model.server
            in
            ( { model | server = { server | url = url } }
            , Cmd.none
            )

        SetRoom room ->
            let
                server =
                    model.server
            in
            ( { model | server = { server | room = room } }
            , Cmd.none
            )

        Connect ->
            case model.gameStatus of
                NotPlayingYet ->
                    ( { model | gameStatus = WaitingForStart }
                    , Protocol.send model.server Protocol.JoinMessage
                    )

                WaitingForStart ->
                    Debug.crash "Connect Msg when model.GameType = WaitingForStart"

                InGame ->
                    Debug.crash "Connect Msg when model.GameType = InGame"

        Recv messageStr ->
            case Decode.decodeString Protocol.decodeNetworkMessage messageStr of
                Err err ->
                    Debug.crash ("Recv decoding failed: " ++ err)

                Ok message ->
                    messageRecieved model message.payload


messageRecieved : Model -> Protocol.Message -> ( Model, Cmd Msg )
messageRecieved model message =
    let
        newGameModel : Int -> Model
        newGameModel seed =
            let
                game =
                    model.game
            in
            { model
                | gameStatus = InGame
                , game = { game | randomSeed = Random.initialSeed seed }
            }
    in
    case ( model.gameStatus, message ) of
        ( NotPlayingYet, _ ) ->
            Debug.crash "Recv when game state is NotPlayingYet"

        ( WaitingForStart, JoinMessage ) ->
            let
                startMsg : Protocol.Message
                startMsg =
                    StartGameMessage { seed = model.startSeed }
            in
            ( newGameModel model.startSeed
            , Protocol.send model.server startMsg
            )

        ( WaitingForStart, StartGameMessage { seed } ) ->
            let
                newModel =
                    newGameModel seed
            in
            ( { newModel | currentPlayer = Player2 }
            , Cmd.none
            )

        ( InGame, TurnMessage { commands } ) ->
            ( opponentEndsRoundOnlineGame model commands
            , Cmd.none
            )

        ( _, _ ) ->
            Debug.crash <|
                "Unexpected gameStatus/Msg combination "
                    ++ toString model.gameStatus
                    ++ " / "
                    ++ toString message


{-| When the user clicks the end turn button.
-}
endRoundOnlineGame : Model -> ( Model, Cmd Msg )
endRoundOnlineGame model =
    let
        ( immediateMoves, _ ) =
            splitPlannedMoves model.plannedMoves

        newModel =
            case model.enemyCommands of
                Nothing ->
                    { model | turnComplete = True }

                Just enemyCommands ->
                    resolveOnlineGameTurn model enemyCommands

        send =
            Protocol.send
                model.server
                (TurnMessage
                    { commands =
                        { moves = immediateMoves
                        , buildOrders = model.buildOrders
                        }
                    }
                )
    in
    ( newModel, send )


opponentEndsRoundOnlineGame : Model -> Commands -> Model
opponentEndsRoundOnlineGame model enemyCommands =
    if model.turnComplete then
        resolveOnlineGameTurn model enemyCommands
    else
        { model | enemyCommands = Just enemyCommands }


resolveOnlineGameTurn : Model -> Commands -> Model
resolveOnlineGameTurn model enemyCommands =
    let
        ( immediateMoves, laterMoves ) =
            splitPlannedMoves model.plannedMoves

        mergedMoves =
            Dict.union immediateMoves enemyCommands.moves

        mergedBuildOrders =
            Dict.union model.buildOrders enemyCommands.buildOrders

        ( reports, newGameState ) =
            Game.resolveTurn
                { moves = mergedMoves
                , buildOrders = mergedBuildOrders
                }
                model.game
    in
    { model
        | game = newGameState
        , plannedMoves = removeOrphanMoves newGameState laterMoves
        , buildOrders = Dict.empty
        , turnComplete = False
        , enemyCommands = Nothing
        , selection =
            Nothing

        -- TODO: Need two selections in the future updateSelection newGameState model.selection
        , gameLog = reports ++ model.gameLog
    }


{-| Remove plans to move units that are no longer on the board.
-}
removeOrphanMoves : Game -> Dict Int (List Point) -> Dict Int (List Point)
removeOrphanMoves game moveDict =
    let
        units : Dict Int Point
        units =
            Game.unitDict (Util.unHexGrid game.grid)

        go : Int -> List Point -> Dict Int (List Point) -> Dict Int (List Point)
        go id movePoint acc =
            if Dict.member id units then
                Dict.insert id movePoint acc
            else
                acc
    in
    Dict.foldr go Dict.empty moveDict


{-| Split planned moves into those to be executed this turn and those for later.
-}
splitPlannedMoves : Dict Int (List Point) -> ( Dict Int Point, Dict Int (List Point) )
splitPlannedMoves allMoves =
    let
        go :
            Int
            -> List Point
            -> ( Dict Int Point, Dict Int (List Point) )
            -> ( Dict Int Point, Dict Int (List Point) )
        go unitId unitMoves ( commands, futureMoves ) =
            case unitMoves of
                [] ->
                    ( commands, futureMoves )

                x :: xs ->
                    ( Dict.insert unitId x commands
                    , case xs of
                        [] ->
                            futureMoves

                        _ ->
                            Dict.insert unitId xs futureMoves
                    )
    in
    Dict.foldr go ( Dict.empty, Dict.empty ) allMoves


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
                                case List.head (Game.friendlyUnits model.currentPlayer tile) of
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


stopBuilding : Model -> Model
stopBuilding model =
    case Model.focusPoint model of
        Nothing ->
            model

        Just point ->
            { model
                | buildOrders =
                    Dict.remove point
                        model.buildOrders
            }


buildOrder : Model -> Buildable -> Model
buildOrder model buildable =
    case Model.focusPoint model of
        Nothing ->
            model

        Just point ->
            case Game.habitatFromPoint point (Util.unHexGrid model.game.grid) of
                Nothing ->
                    model

                Just hab ->
                    { model | buildOrders = Dict.insert point buildable model.buildOrders }
