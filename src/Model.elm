module Model exposing (..)

import Dict exposing (Dict)
import Game exposing (Commands)
import Game.Combat exposing (BattleReport)
import Game.Id as Id exposing (Id(..), IdSeed(..))
import Game.State exposing (Buildable(..), Game, Tile)
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


type alias Model =
    { game : Game
    , gameStatus : GameType
    , plannedMoves :
        Dict Int (List Point)

    -- Keys are unit IDs.
    --
    -- It would be better for type safety to store build orders as a field of
    -- habitats (that way a sea tile couldn't have a build order).
    -- Unfortunately since habitats are defined and stored in the Game part
    -- of the code they don't know about UI things like build orders.
    , buildOrders : Dict Point Buildable
    , turnComplete : Bool
    , enemyCommands : Maybe Commands
    , selection : Maybe Selection
    , hoverPoint : Maybe Point
    , gameLog : List BattleReport

    -- The player controlling the UI:
    , currentPlayer : Player
    , startSeed : Int
    , server : Protocol.Server
    }


init : Int -> Model
init startSeed =
    { game = Game.State.init
    , gameStatus = NotPlayingYet
    , plannedMoves = Dict.empty
    , buildOrders = Dict.empty
    , turnComplete = False
    , enemyCommands = Nothing
    , selection = Nothing
    , hoverPoint = Nothing
    , gameLog = []
    , currentPlayer = Player1
    , startSeed = startSeed
    , server =
        { url = "ws://45.33.68.74:16000"
        , room = "hello"
        }
    }


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
