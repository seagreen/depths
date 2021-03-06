module Update exposing (..)

import Delay
import Dict exposing (Dict)
import Game
import Game.State as Game exposing (Game)
import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Type.Commands exposing (Commands)
import Game.Type.Geology as Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id as Id exposing (Id(..), IdSeed(..), unId)
import Game.Type.Player exposing (Player(..))
import Game.Type.Unit exposing (Unit)
import HexGrid exposing (HexGrid(..), Point)
import Model
    exposing
        ( GameType(..)
        , Model
        , Screen(..)
        , Selection(..)
        , TurnStatus(..)
        )
import Protocol exposing (Message(..), NetworkMessage)
import Random.Pcg as Random
import Time
import Util


type Msg
    = NoOp
      -- When both players commands have been queued.
    | Enter
    | EndTurnButton
    | FinishLoading
      -- When a point is clicked on the board.
      --
      -- This is more complicated than SelectUnit
      -- or SelectTile (which are for clicking the help
      -- boxes for subs or cities respectively) since it
      -- can also do things like unselect the point
      -- if it's already selected.
    | ChangeScreen Screen
    | SelectPoint Point
    | SelectUnit Id
    | SelectTile Point
    | PlanMoves Id (List Point)
    | CancelMove Id
    | BuildOrder Buildable
    | StopBuilding
    | NameEditorFull Id String
    | NameEditorAbbreviation Id String
    | NameEditorSubmit Id
    | SplashScreen SplashScreenMsg
      -- Receive a message from the server
    | Protocol (Result String Protocol.NetworkMessage)


{-| Messages that are only sent when the splash screen is visible.
-}
type
    SplashScreenMsg
    -- Handle changes to the "server" text box before starting a game
    = SetServerUrl String
      -- Handle changes to the "room" text box before starting a game
    | SetRoom String
      -- Handle connecting to a server
    | Connect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Always ignore NoOp messages.
        NoOp ->
            ( model, Cmd.none )

        -- 'Enter' was pressed. If we're in a game, this ends our current turn.
        -- Otherwise, we ignore the message.
        Enter ->
            case model.gameStatus of
                Model.NotPlayingYet ->
                    startGame model

                Model.WaitingForStart ->
                    ( model, Cmd.none )

                Model.InGame ->
                    youEndTurn model

        -- 'End Turn' was pressed; end our current turn.
        EndTurnButton ->
            youEndTurn model

        -- We sent ourselves a 'FinishLoading' message intended to transition
        -- us from 'TurnLoading' back to 'TurnInProgress'.
        FinishLoading ->
            ( case model.turnStatus of
                TurnLoading ->
                    { model | turnStatus = TurnInProgress }

                TurnInProgress ->
                    model

                TurnComplete ->
                    model
            , Cmd.none
            )

        ChangeScreen screen ->
            ( { model | screen = screen }, Cmd.none )

        SelectPoint point ->
            ( { model | selection = newSelection model point }, Cmd.none )

        SelectUnit id ->
            ( { model | selection = Just (SelectedId id) }, Cmd.none )

        SelectTile point ->
            ( { model | selection = Just (SelectedPoint point) }, Cmd.none )

        PlanMoves id points ->
            unlessTurnOver
                model
                ( { model | plannedMoves = Dict.insert (Id.unId id) points model.plannedMoves }
                , Cmd.none
                )

        CancelMove id ->
            unlessTurnOver
                model
                ( { model | plannedMoves = Dict.remove (Id.unId id) model.plannedMoves }
                , Cmd.none
                )

        BuildOrder buildable ->
            unlessTurnOver
                model
                ( buildOrder model buildable, Cmd.none )

        StopBuilding ->
            unlessTurnOver
                model
                ( stopBuilding model, Cmd.none )

        NameEditorFull habId new ->
            ( setHabitatNameEditor model habId (\name -> { name | full = new })
            , Cmd.none
            )

        NameEditorAbbreviation habId new ->
            ( setHabitatNameEditor model habId (\name -> { name | abbreviation = new })
            , Cmd.none
            )

        NameEditorSubmit (Id habId) ->
            ( case Dict.get habId model.habitatNameEditors of
                Nothing ->
                    model

                Just (Habitat.NameEditor new) ->
                    { model
                        | habitatNameEditors =
                            Dict.remove habId model.habitatNameEditors
                        , habitatNamings =
                            Dict.insert habId new model.habitatNamings
                    }
            , Cmd.none
            )

        SplashScreen msg_ ->
            updateSplashScreen msg_ model

        Protocol (Err err) ->
            Model.crash model ("Recv decoding failed: " ++ err)

        Protocol (Ok msg_) ->
            updateProtocol msg_ model


{-| Handle 'SplashScreenMsg'
-}
updateSplashScreen : SplashScreenMsg -> Model -> ( Model, Cmd msg )
updateSplashScreen msg model =
    case msg of
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
                    startGame model

                WaitingForStart ->
                    Model.crash model "Connect Msg when model.GameType = WaitingForStart"

                InGame ->
                    Model.crash model "Connect Msg when model.GameType = InGame"


startGame : Model -> ( Model, Cmd msg )
startGame model =
    ( { model | gameStatus = WaitingForStart }
    , Protocol.send model.server Protocol.JoinMessage
    )


{-| Handle 'Protocol.Message'.
-}
updateProtocol : Protocol.NetworkMessage -> Model -> ( Model, Cmd Msg )
updateProtocol { topic, payload } model =
    let
        newGameModel : Random.Seed -> Model
        newGameModel seed =
            let
                game =
                    model.game
            in
            { model
                | gameStatus = InGame
                , game = { game | randomSeed = seed }
            }

        runUpdate : ( Model, Cmd Msg )
        runUpdate =
            case ( model.gameStatus, payload ) of
                ( NotPlayingYet, _ ) ->
                    Model.crash model "Recv when game state is NotPlayingYet"

                ( WaitingForStart, JoinMessage ) ->
                    let
                        ourSeed : Random.Seed
                        ourSeed =
                            model.game.randomSeed

                        startMsg : Protocol.Message
                        startMsg =
                            StartGameMessage { seed = ourSeed }
                    in
                    ( newGameModel ourSeed
                    , Protocol.send model.server startMsg
                    )

                ( WaitingForStart, StartGameMessage { seed } ) ->
                    let
                        newModel =
                            newGameModel seed
                    in
                    ( { newModel | player = Player2 }
                    , Cmd.none
                    )

                ( InGame, TurnMessage { commands } ) ->
                    opponentEndsTurn model commands

                ( _, _ ) ->
                    Model.crash model <|
                        "Unexpected gameStatus/Msg combination "
                            ++ toString model.gameStatus
                            ++ " / "
                            ++ toString payload
    in
    if topic == model.server.room then
        runUpdate
    else
        ( model, Cmd.none )


unlessTurnOver : Model -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
unlessTurnOver model action =
    case model.turnStatus of
        TurnLoading ->
            action

        TurnInProgress ->
            action

        TurnComplete ->
            ( model, Cmd.none )


youEndTurn : Model -> ( Model, Cmd Msg )
youEndTurn model =
    let
        ( immediateMoves, _ ) =
            splitPlannedMoves model.plannedMoves

        send : Cmd Msg
        send =
            Protocol.send
                model.server
                (TurnMessage
                    -- This is what you send over to your opponent.
                    { commands =
                        { moves = immediateMoves
                        , buildOrders = model.buildOrders
                        , habitatNamings = model.habitatNamings
                        }
                    }
                )
    in
    case model.turnStatus of
        TurnLoading ->
            ( model, Cmd.none )

        TurnComplete ->
            ( model, Cmd.none )

        TurnInProgress ->
            case model.enemyCommands of
                Nothing ->
                    ( { model | turnStatus = TurnComplete }
                    , send
                    )

                Just enemyCommands ->
                    let
                        ( newModel, cmd ) =
                            runResolveTurn model enemyCommands
                    in
                    ( newModel
                    , Cmd.batch
                        [ cmd
                        , send
                        ]
                    )


opponentEndsTurn : Model -> Commands -> ( Model, Cmd Msg )
opponentEndsTurn model enemyCommands =
    let
        handleMsg =
            case model.turnStatus of
                TurnLoading ->
                    ( { model | enemyCommands = Just enemyCommands }
                    , Cmd.none
                    )

                TurnInProgress ->
                    ( { model | enemyCommands = Just enemyCommands }
                    , Cmd.none
                    )

                TurnComplete ->
                    runResolveTurn model enemyCommands
    in
    case model.enemyCommands of
        Nothing ->
            handleMsg

        Just _ ->
            Model.crash model "Oppenent sent two messages in the same turn."


runResolveTurn : Model -> Commands -> ( Model, Cmd Msg )
runResolveTurn model enemyCommands =
    let
        delayThenRemoveLoading : Cmd Msg
        delayThenRemoveLoading =
            Delay.after 500 Time.millisecond FinishLoading

        ( immediateMoves, laterMoves ) =
            splitPlannedMoves model.plannedMoves

        mergedMoves : Dict Int Point
        mergedMoves =
            Dict.union immediateMoves enemyCommands.moves

        mergedBuildOrders : Dict Point Buildable
        mergedBuildOrders =
            Dict.union model.buildOrders enemyCommands.buildOrders

        mergedHabitatNamings : Dict Int Habitat.Name
        mergedHabitatNamings =
            Dict.union model.habitatNamings enemyCommands.habitatNamings

        ( reports, newGameState ) =
            Game.resolveTurn
                -- This is the combination of you and your opponents moves
                -- that's used to roll over into the next turn.
                { moves = mergedMoves
                , buildOrders = mergedBuildOrders
                , habitatNamings = mergedHabitatNamings
                }
                model.game
    in
    -- Be careful not to pass model.game to any of the function
    -- that update UI state based on the new game state.
    -- That's the old game state, the new game state is newGameState.
    ( { model
        | game = newGameState
        , selection = updateSelection newGameState model.selection
        , turnStatus = TurnLoading
        , plannedMoves = removeOrphanMoves newGameState laterMoves
        , buildOrders = Dict.empty
        , habitatNameEditors =
            addEditorsForNewHabitats
                model.player
                newGameState
                model.habitatNameEditors
        , habitatNamings = Dict.empty
        , enemyCommands = Nothing
        , gameLog = reports ++ model.gameLog
      }
    , delayThenRemoveLoading
    )


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
        stillActive : Id -> Maybe ( Point, Unit )
        stillActive id =
            Game.findUnit id (Util.unHexGrid game.grid)

        maybeBecameHabitat : Id -> Maybe Selection
        maybeBecameHabitat id =
            let
                (HexGrid _ grid) =
                    game.grid
            in
            grid
                |> Game.habitatDict
                |> Dict.toList
                |> (\habList ->
                        case List.filter (\( _, hab ) -> hab.id == id) habList of
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
                                case List.head (Game.friendlyUnits model.player tile) of
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


setHabitatNameEditor : Model -> Id -> (Habitat.Name -> Habitat.Name) -> Model
setHabitatNameEditor model habId updateName =
    let
        set (Habitat.NameEditor name) =
            Habitat.NameEditor (updateName name)
    in
    { model
        | habitatNameEditors =
            Dict.update (unId habId) (Maybe.map set) model.habitatNameEditors
    }


addEditorsForNewHabitats :
    Player
    -> Game
    -> Dict Int Habitat.NameEditor
    -> Dict Int Habitat.NameEditor
addEditorsForNewHabitats player game editors =
    let
        (HexGrid _ grid) =
            game.grid

        addIfNew :
            Point
            -> Habitat
            -> Dict Int Habitat.NameEditor
            -> Dict Int Habitat.NameEditor
        addIfNew point hab editorsAcc =
            case hab.name of
                Just _ ->
                    editorsAcc

                Nothing ->
                    if hab.player == player then
                        Dict.update
                            (unId hab.id)
                            (Just << Maybe.withDefault Habitat.emptyNameEditor)
                            editorsAcc
                    else
                        editorsAcc
    in
    Dict.foldl addIfNew editors (Game.habitatDict grid)
