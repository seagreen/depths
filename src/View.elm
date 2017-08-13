module View exposing (..)

-- Core

import Dict exposing (Dict)
import Json.Encode
import Set exposing (Set)
import String


-- 3rd

import Either exposing (Either(..))
import HexGrid exposing (HexGrid(..), Point)
import Html exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Svg exposing (Svg)
import Svg.Attributes as Sattr
import Svg.Events as Sevent exposing (onClick, onMouseOver, onMouseOut)


-- Local

import Game exposing (BattleEvent(..), BattleReport, Outcome(..))
import Game.Building as Building exposing (Building(..))
import Game.Id as Id
import Game.State as Game
    exposing
        ( Buildable(..)
        , Game
        , Geology(..)
        , Habitat
        , HabitatEditor(..)
        , HabitatName
        , Tile
        )
import Game.Unit as Unit exposing (Unit, Player(..), Submarine(..))
import Model exposing (Model, Msg(..), Selection(..))
import Util


type alias BoardInfo =
    { model : Model
    , layout : HexGrid.Layout
    , friendlyPlannedMoves : Set Point
    , pointsReachable : Set Point
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
            model.plannedMoves
                |> Game.unCommands
                |> Dict.values
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

        pointsReachable : Set Point
        pointsReachable =
            case selectedUnit of
                Nothing ->
                    Set.empty

                Just ( point, unit ) ->
                    Set.empty
                        |> HexGrid.reachable point (Unit.stats unit.class).speed
                        |> Set.remove point

        boardInfo : BoardInfo
        boardInfo =
            { model = model
            , layout = layout
            , friendlyPlannedMoves = friendlyPlannedMoves
            , pointsReachable = pointsReachable
            , selectedUnit = selectedUnit
            }
    in
        Svg.svg
            []
            (List.map (renderPoint boardInfo) (Dict.toList dict))


getAbbreviation : Tile -> String
getAbbreviation tile =
    case Game.habitatFromTile tile of
        Just hab ->
            Game.habitatAbbreviation hab

        Nothing ->
            case Game.friendlyUnits tile of
                [] ->
                    ""

                [ unit ] ->
                    (Unit.stats unit.class).abbreviation

                _ ->
                    "**"


renderPoint : BoardInfo -> ( Point, Tile ) -> Html Msg
renderPoint bi ( point, tile ) =
    let
        ( centerX, centerY ) =
            HexGrid.hexToPixel bi.layout point

        corners =
            HexGrid.polygonCorners bi.layout point
    in
        Svg.g
            [ onClick <|
                case bi.selectedUnit of
                    Nothing ->
                        SelectPoint point

                    Just ( _, unit ) ->
                        if Set.member point bi.pointsReachable then
                            PlanMove unit.id point
                        else
                            SelectPoint point
            , onMouseOut EndHover
            , onMouseOver (HoverPoint point)
            ]
            (viewPolygon bi.model tile bi.friendlyPlannedMoves corners point
                :: (tileText centerX
                        centerY
                        (getAbbreviation tile)
                        (case tile.fixed of
                            Mountain (Just hab) ->
                                case Game.productionUntilCompletion hab of
                                    Nothing ->
                                        ""

                                    Just remaining ->
                                        toString remaining

                            _ ->
                                if Set.member point bi.pointsReachable then
                                    "1"
                                else
                                    ""
                        )
                   )
            )


cornersToStr : List ( Float, Float ) -> String
cornersToStr corners =
    corners
        |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
        |> String.join " "


friendlyMoved : Model -> Tile -> Bool
friendlyMoved model tile =
    tile
        |> Game.friendlyUnits
        |> List.any
            (\unit ->
                Dict.member
                    (Id.unId unit.id)
                    (Game.unCommands model.plannedMoves)
            )


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
                            case Game.friendlyUnits tile of
                                [] ->
                                    White

                                _ ->
                                    Red
                        else if friendlyMoved model tile then
                            Red
                        else if Set.member point friendlyPlannedMoves then
                            Red
                        else if Just point == model.hoverPoint then
                            Yellow
                        else
                            Blue

                    Mountain Nothing ->
                        if Just (SelectedPoint point) == model.selection then
                            White
                        else if Set.member point friendlyPlannedMoves then
                            Red
                        else if Just point == model.hoverPoint then
                            Yellow
                        else
                            Gray

                    Mountain (Just _) ->
                        if Just (SelectedPoint point) == model.selection then
                            Red
                        else if Just point == model.hoverPoint then
                            Yellow
                        else
                            Green
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
                - case String.length str of
                    1 ->
                        5

                    2 ->
                        10

                    _ ->
                        15
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


viewHabitat : Point -> Habitat -> Html Msg
viewHabitat point hab =
    Html.div
        [ onClick (SelectTile point)
        , class "alert alert-success"
        ]
        [ Html.h4
            []
            [ Html.b
                []
                [ Html.text <| Game.habitatFullName hab ]
            ]
        , productionForm hab
        , Html.p
            []
            [ Html.text "Production: "
            , badge
                [ Html.text <| toString (Building.production hab.buildings)
                ]
            ]
        , Html.p
            []
            [ Html.text "Population: "
            , badge
                [ Html.text <| toString (Building.population hab.buildings) ]
            ]
        , Html.p
            []
            [ Html.text <|
                "Buildings: "
                    ++ (String.concat <|
                            List.intersperse ", " <|
                                List.map toString hab.buildings
                       )
            ]
        ]


productionForm : Habitat -> Html Msg
productionForm hab =
    let
        option : Maybe Buildable -> Html Msg
        option buildable =
            Html.option
                (if buildable == hab.producing then
                    [ Hattr.selected True ]
                 else
                    []
                )
                [ Html.text <|
                    case buildable of
                        Nothing ->
                            "<None>"

                        Just (BuildSubmarine sub) ->
                            toString sub

                        Just (BuildBuilding building) ->
                            toString building
                ]

        msgFromString : String -> Msg
        msgFromString s =
            if s == "<None>" then
                BuildOrder Nothing
            else
                case Building.fromString s of
                    Just building ->
                        BuildOrder (Just (BuildBuilding building))

                    Nothing ->
                        case Unit.fromString s of
                            Nothing ->
                                NoOp

                            Just sub ->
                                BuildOrder (Just (BuildSubmarine sub))
    in
        Html.form
            [ class "form-inline"
            , Hattr.name "foo"
            ]
            [ Html.div
                [ class "form-group" ]
                [ Html.label
                    [ Hattr.for "constructing" ]
                    [ Svg.text <|
                        "Constructing"
                            ++ (case Game.productionUntilCompletion hab of
                                    Nothing ->
                                        ""

                                    Just toGo ->
                                        " (" ++ toString toGo ++ " production to go" ++ ")"
                               )
                            -- Non-breaking space to separate the label from the box.
                            -- The Bootstrap examples have this separation
                            -- automatically, not sure what I'm doing wrong.
                            ++
                                ": "
                    ]
                , Html.select
                    [ class "form-control"
                    , Hattr.id "constructing"
                    , Hevent.onInput msgFromString
                    ]
                    [ option Nothing
                    , case Unit.buildable hab.buildings of
                        [] ->
                            Html.text ""

                        unitChoices ->
                            Html.optgroup
                                [ label_ "Units" ]
                                (List.map (option << Just << BuildSubmarine) unitChoices)
                    , case Building.buildable hab.buildings of
                        [] ->
                            Html.text ""

                        buildingChoices ->
                            Html.optgroup
                                [ label_ "Buildings" ]
                                (List.map (option << Just << BuildBuilding) buildingChoices)
                    ]
                ]
            ]


viewHabitatNameForm : HabitatEditor -> Svg Msg
viewHabitatNameForm (HabitatEditor editor) =
    Html.div
        [ class "alert alert-warning" ]
        [ Html.form
            [ Hevent.onSubmit NameEditorSubmit ]
            [ Html.h4
                []
                [ Html.b
                    []
                    [ Html.text "Name Habitat" ]
                ]
            , Html.div
                [ class "form-group" ]
                [ Html.label
                    [ Hattr.for "habitatName" ]
                    [ Svg.text "Full name:" ]
                , Html.input
                    [ class "form-control"
                    , Hattr.type_ "text"
                    , Hattr.id "habitatName"
                    , Hevent.onInput NameEditorFull
                    , Hattr.value editor.full
                    ]
                    []
                ]
            , Html.div
                [ class "form-group" ]
                [ Html.label
                    [ Hattr.for "habitatAbbreviation" ]
                    [ Svg.text "Abbreviation (1-3 letters):" ]
                , Html.input
                    [ class "form-control"
                    , Hattr.type_ "text"
                    , Hattr.id "habitatAbbreviation"
                    , Hevent.onInput NameEditorAbbreviation
                    , Hattr.value editor.abbreviation
                    ]
                    []
                ]
            , Html.button
                [ Hattr.type_ "submit" ]
                [ Svg.text "Found" ]
            ]
        ]


viewUnit : Maybe Selection -> Unit -> Html Msg
viewUnit selection unit =
    let
        stats =
            Unit.stats unit.class
    in
        Html.div
            [ onClick (SelectUnit unit.id)
            , class <|
                "alert alert-success"
                    ++ if Just (SelectedId unit.id) == selection then
                        " focused"
                       else
                        ""
            ]
            [ Html.h4
                []
                [ Html.b
                    []
                    [ Html.text stats.name ]
                ]
            , case stats.helpText of
                Nothing ->
                    Html.text ""

                Just help ->
                    Html.p
                        []
                        [ Html.text help ]
            , Html.p
                []
                [ Html.text "Sensors: "
                , badge
                    [ Html.text <| toString stats.sensors ]
                ]
            , Html.p
                []
                [ Html.text "Stealth: "
                , badge
                    [ Html.text <| toString stats.stealth ]
                ]
            , Html.p
                []
                [ Html.text "Firepower: "
                , badge
                    [ Html.text <| toString stats.firepower ]
                ]
            ]


view : Model -> Svg Msg
view model =
    let
        (HexGrid _ dict) =
            model.game.grid
    in
        Html.div
            []
            [ Html.h1 [] [ Html.text "FPG: The Depths" ]
            , Html.div
                [ class "row" ]
                [ Html.div
                    [ class "col-lg-5" ]
                    [ Html.p
                        []
                        [ Html.a
                            [ Hattr.href "https://github.com/seagreen/fpg-depths#user-guide" ]
                            -- Use label instead of button to prevent button from staying focused after
                            -- (a) right clicking it to open the link in a new window
                            -- or (b) clicking it and then hitting the back button.
                            --
                            -- Idea from: https://stackoverflow.com/a/34051869
                            [ Html.label
                                [ class "btn btn-default"
                                , Hattr.type_ "button"
                                ]
                                [ Html.text "User Guide (on GitHub)" ]
                            ]
                        ]
                    , Html.p
                        []
                        [ Html.text "Turn "
                        , badge
                            [ Html.text (toString (Game.unTurn model.game.turn)) ]
                        ]
                    , displayOutcome model.game
                    , displayBattleReports model
                    , case Model.focus model of
                        Nothing ->
                            Html.text ""

                        Just ( point, tile ) ->
                            Html.div
                                []
                                [ case tile.fixed of
                                    Mountain (Just hab) ->
                                        Html.div
                                            []
                                            [ viewHabitat point hab
                                            , case hab.name of
                                                Right _ ->
                                                    Html.text ""

                                                Left editor ->
                                                    viewHabitatNameForm editor
                                            ]

                                    _ ->
                                        Html.text ""
                                , Html.div
                                    []
                                    (List.map (viewUnit model.selection) <| Game.friendlyUnits tile)
                                ]
                    , startingHelpMessage model
                    ]
                , Html.div
                    [ class "col-lg-7" ]
                    [ Html.div
                        [ class "text-center" ]
                        [ viewBoard model
                        , endTurnButton model
                        ]
                    ]
                ]
            ]


displayOutcome : Game -> Html Msg
displayOutcome game =
    case Game.outcome game of
        Just Victory ->
            Html.div
                [ class "alert alert-success" ]
                [ Html.text "Glorious victory!" ]

        Just Defeat ->
            Html.div
                [ class "alert alert-danger" ]
                [ Html.text "Terrible defeat!" ]

        Nothing ->
            Html.text ""


displayBattleReports : Model -> Html Msg
displayBattleReports model =
    let
        attackDescription : Maybe Buildable -> String
        attackDescription mBuildable =
            case mBuildable of
                Nothing ->
                    "enemy action."

                Just (BuildBuilding building) ->
                    case building of
                        TorpedoTube ->
                            "habitat-launched torpedoes."

                        _ ->
                            "habitat-based weapons."

                Just (BuildSubmarine sub) ->
                    case sub of
                        RemotelyOperatedVehicle ->
                            "a torpedo from a ROV."

                        AttackSubmarine ->
                            "a torpedo from an attack submarine."

                        _ ->
                            "submarine-based weapons."

        displayEvent : BattleEvent -> Html Msg
        displayEvent event =
            Html.li
                []
                [ case event of
                    DetectionEvent enemy buildable ->
                        Html.text <|
                            "Our "
                                ++ Game.name buildable
                                ++ (case enemy of
                                        BuildSubmarine sub ->
                                            " detected an enemy "
                                                ++ (Unit.stats sub).name

                                        BuildBuilding enemyBuilding ->
                                            " found an enemy "
                                                ++ (Building.stats enemyBuilding).name
                                   )
                                ++ "."

                    DestructionEvent owner destroyed mDestroyer ->
                        Html.text <|
                            (case owner of
                                Human ->
                                    "Our "

                                Computer ->
                                    "Enemy "
                            )
                                ++ Game.name destroyed
                                ++ " was destroyed by "
                                ++ attackDescription mDestroyer
                ]

        displayReport : BattleReport -> Html Msg
        displayReport report =
            Html.div
                [ class "alert alert-danger" ]
                [ Html.h4
                    []
                    [ Html.b
                        []
                        [ Html.text <| "Combat log: " ++ report.habitat ]
                    ]
                , Html.ol
                    []
                    (List.reverse (List.map displayEvent report.events))
                ]
    in
        Html.div
            []
            (List.map displayReport
                (List.filter
                    (\entry -> Game.unTurn entry.turn == Game.unTurn model.game.turn - 1)
                    model.gameLog
                )
            )


endTurnButton : Model -> Html Msg
endTurnButton model =
    case Game.outcome model.game of
        Just _ ->
            Html.text ""

        Nothing ->
            Html.button
                [ onClick EndTurn
                , Hattr.type_ "button"
                , class "btn btn-primary btn-lg"
                ]
                [ Html.text "End turn" ]


startingHelpMessage : Model -> Html Msg
startingHelpMessage model =
    if
        Game.unTurn model.game.turn
            == 1
            && case model.selection of
                Just (SelectedId _) ->
                    False

                _ ->
                    True
    then
        Html.div
            [ class "alert alert-info" ]
            [ Html.text "Click the 'CS' tile to select your first unit." ]
    else
        Html.text ""


type Color
    = Red
    | Green
    | DarkGreen
    | Blue
    | Yellow
    | Black
    | White
    | Gray


{-| https://github.com/elm-lang/html/issues/136
-}
label_ : String -> Html.Attribute msg
label_ s =
    Hattr.property "label" (Json.Encode.string s)


badge : List (Html msg) -> Html msg
badge =
    Html.span [ class "badge" ]


{-| Be careful not to use toString instead.
-}
showColor : Color -> String
showColor a =
    case a of
        Red ->
            "#e74c3c"

        Green ->
            "green"

        DarkGreen ->
            "darkgreen"

        Blue ->
            "#3498db"

        Yellow ->
            "#f1c40f"

        Black ->
            "black"

        White ->
            "white"

        Gray ->
            "darkgrey"
