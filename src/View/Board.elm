module View.Board exposing (..)

import Dict exposing (Dict)
import Game.State as Game exposing (Game, Tile)
import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Type.Geology as Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Unit as Unit exposing (Player(..), Submarine(..), Unit)
import HexGrid exposing (HexGrid(..), Point)
import Html exposing (Html)
import Html.Events as Hevent
import Json.Decode
import Model exposing (GameType(..), Model, Selection(..))
import Set exposing (Set)
import String
import Svg exposing (Svg)
import Svg.Attributes as Sattr
import Svg.Events as Sevent
import Update exposing (Msg(..))
import Util


type alias BoardInfo =
    { model : Model
    , layout : HexGrid.Layout
    , friendlyPlannedMoves : Set Point
    , selectedUnit : Maybe ( Point, Unit )
    }


viewBoard : Model -> Svg Msg
viewBoard model =
    let
        (HexGrid _ dict) =
            model.game.grid

        layout : HexGrid.Layout
        layout =
            HexGrid.mkPointyTop 30 30 (750 / 2) (600 / 2)

        friendlyPlannedMoves : Set Point
        friendlyPlannedMoves =
            let
                friendlyUnits : Dict Int (List Point)
                friendlyUnits =
                    Game.friendlyUnitDict model.currentPlayer (Util.unHexGrid model.game.grid)
                        |> Dict.map (\_ _ -> [])
            in
            Dict.intersect model.plannedMoves friendlyUnits
                |> Dict.values
                |> List.concat
                |> Set.fromList

        selectedUnit : Maybe ( Point, Unit )
        selectedUnit =
            model.selection
                |> Maybe.andThen
                    (\selection ->
                        case selection of
                            SelectedId id ->
                                Game.findUnit id (Util.unHexGrid model.game.grid)

                            _ ->
                                Nothing
                    )

        boardInfo : BoardInfo
        boardInfo =
            { model = model
            , layout = layout
            , friendlyPlannedMoves = friendlyPlannedMoves
            , selectedUnit = selectedUnit
            }
    in
    Svg.svg
        []
        (List.map (renderPoint model boardInfo) (Dict.toList dict))


getAbbreviation : Model -> Point -> Tile -> String
getAbbreviation model point tile =
    case Game.habitatFromTile tile of
        Just hab ->
            if canSeeHabitat model point hab then
                Habitat.abbreviationWithDefault hab
            else
                ""

        Nothing ->
            case Game.friendlyUnits model.currentPlayer tile of
                [] ->
                    ""

                [ unit ] ->
                    (Unit.stats unit.class).abbreviation

                _ ->
                    "**"


movesToPoint : Point -> Point -> Int -> List Point
movesToPoint start end speed =
    let
        maybeStopHere : Int -> Point -> Maybe Point
        maybeStopHere ix point =
            if ix == 0 then
                Nothing
            else if ix % speed == 0 then
                Just point
            else if point == end then
                -- We still want to reach our destination even if the final move
                -- doesn't use our full move speed:
                Just point
            else
                Nothing
    in
    HexGrid.line start end
        |> List.indexedMap maybeStopHere
        |> List.filterMap identity


renderPoint : Model -> BoardInfo -> ( Point, Tile ) -> Html Msg
renderPoint model bi ( point, tile ) =
    let
        ( centerX, centerY ) =
            HexGrid.hexToPixel bi.layout point

        corners =
            HexGrid.polygonCorners bi.layout point

        topText : String
        topText =
            getAbbreviation model point tile

        bottomText : String
        bottomText =
            case tile.fixed of
                Depths ->
                    ""

                Mountain Nothing ->
                    ""

                Mountain (Just hab) ->
                    if hab.player == model.currentPlayer then
                        case getRemainingProduction bi.model point hab of
                            Just remaining ->
                                toString remaining

                            Nothing ->
                                ""
                    else
                        ""
    in
    Svg.g
        [ Sevent.onClick <| SelectPoint point
        , Sattr.class "hex"
        , onRightClick <|
            case bi.selectedUnit of
                Nothing ->
                    NoOp

                Just ( oldPoint, unit ) ->
                    if point == oldPoint then
                        CancelMove unit.id
                    else
                        case movesToPoint oldPoint point (Unit.stats unit.class).speed of
                            [] ->
                                NoOp

                            moves ->
                                PlanMoves unit.id moves
        ]
        (viewPolygon
            bi.model
            tile
            bi.friendlyPlannedMoves
            corners
            point
            :: tileText
                centerX
                centerY
                topText
                bottomText
        )


getRemainingProduction : Model -> Point -> Habitat -> Maybe Int
getRemainingProduction model point hab =
    case ( hab.producing, Dict.get point model.buildOrders ) of
        ( Just producing, Just buildOrder ) ->
            if buildOrder == producing then
                Just (Buildable.cost buildOrder - hab.produced)
            else
                Just (Buildable.cost buildOrder)

        ( Just producing, Nothing ) ->
            Just (Buildable.cost producing - hab.produced)

        ( Nothing, Just buildOrder ) ->
            Just (Buildable.cost buildOrder)

        ( Nothing, Nothing ) ->
            Nothing


cornersToStr : List ( Float, Float ) -> String
cornersToStr corners =
    corners
        |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
        |> String.join " "


viewPolygon :
    Model
    -> Tile
    -> Set Point
    -> List ( Float, Float )
    -> Point
    -> Html msg
viewPolygon model tile friendlyPlannedMoves corners point =
    Svg.polygon
        [ Sattr.points (cornersToStr corners)
        , Sattr.fill <|
            showColor <|
                case tile.fixed of
                    Depths ->
                        if Just point == Model.focusPoint model then
                            case Game.friendlyUnits model.currentPlayer tile of
                                [] ->
                                    White

                                _ ->
                                    Red
                        else if Set.member point friendlyPlannedMoves then
                            DarkBlue
                        else
                            Blue

                    Mountain mHab ->
                        if Just (SelectedPoint point) == model.selection then
                            case mHab of
                                Just hab ->
                                    if canSeeHabitat model point hab then
                                        Red
                                    else
                                        White

                                Nothing ->
                                    White
                        else if Set.member point friendlyPlannedMoves then
                            DarkGray
                        else
                            case mHab of
                                Just hab ->
                                    if canSeeHabitat model point hab then
                                        Green
                                    else
                                        Gray

                                Nothing ->
                                    Gray
        ]
        []


tileText :
    Float
    -> Float
    -> String
    -> String
    -> List (Svg msg)
tileText centerX centerY upperText lowerText =
    let
        centerHorizontally : String -> Float
        centerHorizontally str =
            centerX
                - (case String.length str of
                    1 ->
                        5

                    2 ->
                        10

                    _ ->
                        15
                  )
    in
    [ Svg.text_
        [ Sattr.x <| toString (centerHorizontally upperText)
        , Sattr.y <| toString (centerY - 5)
        ]
        [ Svg.text upperText ]
    , Svg.text_
        [ Sattr.x <| toString (centerHorizontally lowerText)
        , Sattr.y <| toString (centerY + 10)
        ]
        [ Svg.text lowerText ]
    ]


{-| Does the current player have a ship at the given point?
-}
hasShipAtPoint : Model -> Point -> Bool
hasShipAtPoint { currentPlayer, game } point =
    Game.friendlyUnitDict currentPlayer (Util.unHexGrid game.grid)
        |> Dict.values
        |> List.filter ((==) point)
        |> not
        << List.isEmpty


canSeeHabitat : Model -> Point -> Habitat -> Bool
canSeeHabitat model point hab =
    hab.player == model.currentPlayer || hasShipAtPoint model point


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    Hevent.onWithOptions
        "contextmenu"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.succeed msg)


type Color
    = Red
    | DarkRed
    | Green
    | DarkGreen
    | Blue
    | DarkBlue
    | Yellow
    | Black
    | White
    | Gray
    | DarkGray


{-| Be careful not to use toString instead.
-}
showColor : Color -> String
showColor a =
    case a of
        Red ->
            "#e74c3c"

        DarkRed ->
            "darkred"

        Green ->
            "green"

        DarkGreen ->
            "darkgreen"

        Blue ->
            "#3498db"

        DarkBlue ->
            "#3468db"

        Yellow ->
            "#f1c40f"

        Black ->
            "black"

        White ->
            "white"

        Gray ->
            "darkgrey"

        DarkGray ->
            "#636363"
