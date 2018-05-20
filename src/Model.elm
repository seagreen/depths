module Model exposing (..)

import Dict exposing (Dict)
import Game exposing (Commands)
import Game.Combat exposing (BattleReport)
import Game.State exposing (Game)
import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Type.Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id as Id exposing (Id(..), IdSeed(..))
import Game.Type.Player exposing (Player(..))
import Game.Type.Tile exposing (Tile)
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
    { server : Protocol.Server
    , game : Game
    , gameStatus : GameType

    -- Whether the user of this computer controls Player1 or Player2.
    , player : Player
    , screen : Screen
    , selection : Maybe Selection
    , turnStatus : TurnStatus
    , plannedMoves : Dict Int (List Point)

    -- Keys are unit IDs.
    --
    -- It would be better for type safety to store build orders as a field of
    -- habitats (that way a sea tile couldn't have a build order).
    -- Unfortunately since habitats are defined and stored in the Game part
    -- of the code they don't know about UI things like build orders.
    , buildOrders : Dict Point Buildable

    -- Int is the habitat ID.
    , habitatNameEditors : Dict Int Habitat.NameEditor
    , enemyCommands : Maybe Commands
    , gameLog : List BattleReport

    -- Using this instead of Debug.crash because the latter disables
    -- the debugger (see: https://github.com/elm-lang/core/issues/953):
    , crashed : Maybe String
    }


init : Model
init =
    { server =
        { url = "ws://45.33.68.74:16000"
        , room = "hello"
        }
    , game = Game.State.init
    , gameStatus = NotPlayingYet
    , player = Player1
    , screen = Board
    , selection = Nothing
    , turnStatus = TurnInProgress
    , plannedMoves = Dict.empty
    , buildOrders = Dict.empty
    , habitatNameEditors = Dict.empty
    , enemyCommands = Nothing
    , gameLog = []
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


{-| Like `focusPoint`, but also returns the `Tile` at that point.
-}
focus : Model -> Maybe ( Point, Tile )
focus model =
    let
        (HexGrid _ grid) =
            model.game.grid
    in
    focusPoint model
        |> Maybe.andThen
            (\point ->
                Maybe.map
                    (\tile -> ( point, tile ))
                    (Dict.get point grid)
            )


{-| Like `focusPoint`, but returns the `Habitat` at that point.
-}
focusedHabitat : Model -> Maybe Habitat
focusedHabitat model =
    focus model
        |> Maybe.andThen
            (\( _, tile ) ->
                case tile.fixed of
                    Mountain (Just hab) ->
                        Just hab

                    Mountain Nothing ->
                        Nothing

                    Depths ->
                        Nothing
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
