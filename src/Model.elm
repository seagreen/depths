module Model exposing (..)

import Dict exposing (Dict)
import Game exposing (Commands)
import Game.Id as Id exposing (Id(..), IdSeed(..))
import Game.State exposing (Buildable(..), Game, Tile)
import Game.Unit exposing (Player(..))
import HexGrid exposing (Direction, HexGrid(..), Point)
import Util


type Msg
    = NoOp
      -- When both players commands have been queued
    | EndRound
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
    | PlanMoves Id (List Point)
    | CancelMove Id
    | BuildOrder Buildable
    | StopBuilding
    | NameEditorFull String
    | NameEditorAbbreviation String
    | NameEditorSubmit
      -- Handle changes to the "server" text box before starting a game
    | SetServer String
      -- Handle changes to the "room" text box before starting a game
    | SetRoom String
      -- Handle connecting to a server
    | Connect
      -- Receive a message from the server
    | Recv String


type
    GameType
    -- Haven't selected game type yet
    = NotPlayingYet { server : String, room : String }
      -- Two players sharing a browser
    | SharedComputer
    | Online OnlineGame


type alias OnlineGame =
    { server : String
    , room : String
    , state : OnlineGameState
    }


type OnlineGameState
    = WaitingForStart
    | InGame


type alias Model =
    { game : Game
    , gameType : GameType
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
    , gameLog :
        List Game.BattleReport

    -- The player controlling the UI:
    , currentPlayer : Player
    , startSeed : Int
    }


init : Int -> Model
init startSeed =
    { game = Game.State.init
    , gameType = NotPlayingYet { server = "ws://127.0.0.1:8000", room = "hello" }
    , plannedMoves = Dict.empty
    , buildOrders = Dict.empty
    , turnComplete = False
    , enemyCommands = Nothing
    , selection = Nothing
    , hoverPoint = Nothing
    , gameLog = []
    , currentPlayer = Player1
    , startSeed = startSeed
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
