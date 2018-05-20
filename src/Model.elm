module Model exposing (..)

import Dict exposing (Dict)
import Game exposing (Commands)
import Game.Combat exposing (BattleReport)
import Game.Id as Id exposing (Id(..), IdSeed(..))
import Game.State exposing (Game, Tile)
import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Unit exposing (Player(..))
import HexGrid exposing (Direction, HexGrid(..), Point)
import Protocol
import Util


type
    GameType
    -- Haven't selected a server yet
    = NotPlayingYet
    | WaitingForStart
    | InGame


type Screen
    = Board
    | TechTable


type alias Model =
    { game : Game
    , gameStatus : GameType
    , screen : Screen
    , plannedMoves :
        Dict Int (List Point)

    -- Keys are unit IDs.
    --
    -- It would be better for type safety to store build orders as a field of
    -- habitats (that way a sea tile couldn't have a build order).
    -- Unfortunately since habitats are defined and stored in the Game part
    -- of the code they don't know about UI things like build orders.
    , buildOrders : Dict Point Buildable
    , turnStatus : TurnStatus
    , enemyCommands : Maybe Commands
    , selection : Maybe Selection
    , gameLog : List BattleReport

    -- The player controlling the UI:
    , currentPlayer : Player
    , server : Protocol.Server

    -- Using this instead of Debug.crash because the latter disables
    -- the debugger (see: https://github.com/elm-lang/core/issues/953):
    , crashed : Maybe String
    }


init : Model
init =
    { game = Game.State.init
    , gameStatus = NotPlayingYet
    , screen = Board
    , plannedMoves = Dict.empty
    , buildOrders = Dict.empty
    , turnStatus = TurnInProgress
    , enemyCommands = Nothing
    , selection = Nothing
    , gameLog = []
    , currentPlayer = Player1
    , server =
        { url = "ws://45.33.68.74:16000"
        , room = "hello"
        }
    , crashed = Nothing
    }


{-| TurnLoading exists to prevent this confusing situation:

1.  Your opponent ends their turn.
2.  You finish your moves and click "End turn".
3.  The button doesn't change, making it look like your click didn't take effect.

Instead we go to TurnLoading status for a short time,
during which the end turn button will change to something like "loading".

-}
type TurnStatus
    = TurnLoading
    | TurnInProgress
    | TurnComplete


{-| There are two types of selections: (1) SelectionPoints, which happen
when the user clicks a tile, and (2) SelectedIds, which happen when a user
clicks a specific submarine description on the left side of the screen.
-}
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
                        Maybe.map
                            Tuple.first
                            (Game.State.findUnit id (Util.unHexGrid model.game.grid))
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


{-| Use this instead of setting the `crashed` field directly
to prevent overwriting a previous crash message.
-}
crash : Model -> String -> ( Model, Cmd msg )
crash model crashMsg =
    ( case model.crashed of
        Just _ ->
            model

        Nothing ->
            { model | crashed = Just crashMsg }
    , Cmd.none
    )
