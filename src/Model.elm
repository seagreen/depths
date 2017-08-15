module Model exposing (..)

-- Core

import Dict exposing (Dict)
import Random


-- 3rd

import HexGrid exposing (HexGrid(..), Direction, Point)


-- Local

import Game exposing (Commands)
import Game.Id as Id exposing (Id(..), IdSeed(..))
import Game.State exposing (Buildable(..), Game, Tile)
import Util


type Msg
    = NoOp
    | SetRandomSeed NewSeed
    | EndTurn
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
    | BuildOrder (Maybe Buildable)
    | NameEditorFull String
    | NameEditorAbbreviation String
    | NameEditorSubmit


type alias Model =
    { game : Game
    , plannedMoves :
        Dict Int (List Point)
        -- Keys are unit IDs.
    , buildOrders : Dict Point (Maybe Buildable)
    , selection : Maybe Selection
    , hoverPoint : Maybe Point
    , gameLog : List Game.BattleReport
    }


init : Model
init =
    { game = Game.State.init
    , plannedMoves = Dict.empty
    , buildOrders = Dict.empty
    , selection = Nothing
    , hoverPoint = Nothing
    , gameLog = []
    }


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


type NewSeed
    = NewSeed Int


newRandomSeed : Cmd Msg
newRandomSeed =
    Random.generate
        (SetRandomSeed << NewSeed)
        -- Using 999999 instead of Random.maxInt here for more human-sized values
        -- (to make them more readable in the debugger and that kind of thing).
        (Random.int 0 999999)
