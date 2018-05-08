module View exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Game exposing (BattleEvent(..), BattleReport, Outcome(..))
import Game.Building as Building exposing (Building(..))
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
import Game.Unit as Unit exposing (Player(..), Submarine(..), Unit)
import HexGrid exposing (HexGrid(..), Point)
import Html exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Json.Decode
import Json.Encode
import Model exposing (GameType(..), Model, Msg(..), Selection(..))
import Set exposing (Set)
import String
import Svg exposing (Svg)
import Svg.Attributes as Sattr
import Svg.Events as Sevent exposing (onClick, onMouseOut, onMouseOver)
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
        (List.map (renderPoint model.currentPlayer boardInfo) (Dict.toList dict))


getAbbreviation : Player -> Tile -> String
getAbbreviation currentPlayer tile =
    case Game.habitatFromTile tile of
        Just hab ->
            Game.habitatAbbreviation hab

        Nothing ->
            case Game.friendlyUnits currentPlayer tile of
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


renderPoint : Player -> BoardInfo -> ( Point, Tile ) -> Html Msg
renderPoint player bi ( point, tile ) =
    let
        ( centerX, centerY ) =
            HexGrid.hexToPixel bi.layout point

        corners =
            HexGrid.polygonCorners bi.layout point

        topText : String
        topText =
            getAbbreviation player tile

        bottomText : String
        bottomText =
            case tile.fixed of
                Mountain (Just hab) ->
                    case viewRemainingProduction bi.model point hab of
                        Nothing ->
                            ""

                        Just remaining ->
                            toString remaining

                _ ->
                    ""
    in
    Svg.g
        [ onClick <| SelectPoint point
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
        , onMouseOut EndHover
        , onMouseOver (HoverPoint point)
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


viewRemainingProduction : Model -> Point -> Habitat -> Maybe Int
viewRemainingProduction model point hab =
    case ( hab.producing, Dict.get point model.buildOrders ) of
        ( Just producing, Just buildOrder ) ->
            if buildOrder == producing then
                Just (Game.cost buildOrder - hab.produced)
            else
                Just (Game.cost buildOrder)

        ( Just producing, Nothing ) ->
            Just (Game.cost producing - hab.produced)

        ( Nothing, Just buildOrder ) ->
            Just (Game.cost buildOrder)

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
                        else if Just point == model.hoverPoint then
                            Yellow
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
                        else if Just point == model.hoverPoint then
                            Yellow
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


viewHabitat : Model -> Point -> Habitat -> Html Msg
viewHabitat model point hab =
    if not (canSeeHabitat model point hab) then
        Html.text ""
    else
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
            , productionForm model point hab
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


{-| Generic onchange handler for <select>
-}
onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    Hevent.on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string


productionForm : Model -> Point -> Habitat -> Html Msg
productionForm model point hab =
    let
        costFromBuildable : Buildable -> Int
        costFromBuildable buildable =
            case buildable of
                BuildSubmarine sub ->
                    Unit.stats sub |> .cost

                BuildBuilding building ->
                    Building.stats building |> .cost

        option : Maybe Buildable -> Html Msg
        option buildable =
            let
                buildableStr =
                    Maybe.map
                        (\buildable ->
                            case buildable of
                                BuildSubmarine sub ->
                                    toString sub

                                BuildBuilding building ->
                                    toString building
                        )
                        buildable
                        |> Maybe.withDefault "<None>"
            in
            Html.option
                (if buildable == hab.producing then
                    [ Hattr.selected True, Hattr.value buildableStr ]
                 else
                    [ Hattr.value buildableStr ]
                )
                [ Html.text <|
                    case buildable of
                        Nothing ->
                            buildableStr

                        Just buildable ->
                            buildableStr ++ " (" ++ toString (costFromBuildable buildable) ++ ")"
                ]

        msgFromString : String -> Msg
        msgFromString s =
            if s == "<None>" then
                StopBuilding
            else
                case Building.fromString s of
                    Just building ->
                        BuildOrder (BuildBuilding building)

                    Nothing ->
                        case Unit.fromString s of
                            Nothing ->
                                NoOp

                            Just sub ->
                                BuildOrder (BuildSubmarine sub)
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
                        ++ (case viewRemainingProduction model point hab of
                                Nothing ->
                                    ""

                                Just toGo ->
                                    " (" ++ toString toGo ++ " production to go" ++ ")"
                           )
                        -- Non-breaking space to separate the label from the box.
                        -- The Bootstrap examples have this separation
                        -- automatically, not sure what I'm doing wrong.
                        ++ ": "
                ]

            -- TODO: This should be using Html.on "change" instead of using strings
            , Html.select
                [ class "form-control"
                , Hattr.id "constructing"
                , onChange msgFromString
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
                    , Hattr.maxlength 3
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
                ++ (if Just (SelectedId unit.id) == selection then
                        " focused"
                    else
                        ""
                   )
        ]
        [ Html.h4
            []
            [ Html.b
                []
                [ Html.text stats.name ]
            ]
        , Maybe.withDefault (Html.text "") (Unit.helpText unit.class)
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


view : Model -> Html Msg
view model =
    case model.gameStatus of
        NotPlayingYet ->
            Html.div
                []
                [ Html.input
                    [ Hattr.placeholder "Server"
                    , Hattr.value model.server.url
                    , Hevent.onInput SetServerUrl
                    ]
                    []
                , Html.input
                    [ Hattr.placeholder "Room"
                    , Hattr.value model.server.room
                    , Hevent.onInput SetRoom
                    ]
                    []
                , Html.button [ Hevent.onClick Connect ] [ Html.text "CONNECT!!" ]
                ]

        WaitingForStart ->
            Html.div [] [ Html.text "Waiting for other player." ]

        InGame ->
            Html.div [] [ viewGame model ]


viewGame : Model -> Html Msg
viewGame model =
    let
        (HexGrid _ dict) =
            model.game.grid
    in
    Html.div
        []
        [ Html.h1 [] [ Html.text "FPG: The Depths" ]
        , Html.div []
            [ Html.text <|
                "Player "
                    ++ (if model.currentPlayer == Player1 then
                            "1"
                        else
                            "2"
                       )
                    ++ "'s turn"
            ]
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
                                    -- can only build on mountains
                                    Html.div
                                        []
                                        [ viewHabitat model point hab
                                        , case hab.name of
                                            Right _ ->
                                                Html.text ""

                                            Left editor ->
                                                if hab.player == model.currentPlayer then
                                                    viewHabitatNameForm editor
                                                else
                                                    Html.text ""
                                        ]

                                _ ->
                                    Html.text ""
                            , Html.div
                                []
                                (List.map (viewUnit model.selection) <| Game.friendlyUnits model.currentPlayer tile)
                            ]
                , startingHelpMessage model
                ]
            , Html.div
                [ class "col-lg-7" ]
                [ Html.div
                    [ class "text-center" ]
                    [ Svg.svg
                        []
                        [ viewBoard model ]
                    , endTurnButton model
                    ]
                ]
            ]
        ]


displayOutcome : Game -> Html Msg
displayOutcome game =
    case Game.outcome game of
        Just (Victory player) ->
            Html.div
                [ class "alert alert-success" ]
                [ Html.text <| "Glorious victory to " ++ toString player ++ "!" ]

        Just Draw ->
            Html.div
                [ class "alert alert-danger" ]
                [ Html.text "Draws are just losses on both sides." ]

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
                            (if owner == model.currentPlayer then
                                "Our "
                             else
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
            if model.turnComplete then
                Html.button
                    [ Hattr.type_ "button"
                    , class "btn btn-default btn-lg"
                    ]
                    [ Html.text "(Waiting)" ]
            else
                Html.button
                    [ onClick EndRound
                    , Hattr.type_ "button"
                    , class "btn btn-primary btn-lg"
                    ]
                    [ Html.text "End turn" ]


startingHelpMessage : Model -> Html Msg
startingHelpMessage model =
    if
        Game.unTurn model.game.turn
            == 1
            && (case model.selection of
                    Just (SelectedId _) ->
                        False

                    _ ->
                        True
               )
    then
        Html.div
            [ class "alert alert-info" ]
            [ Html.text "Click the 'CS' tile to select your first unit." ]
    else
        Html.text ""


onRightClick : msg -> Html.Attribute msg
onRightClick msg =
    Hevent.onWithOptions
        "contextmenu"
        { stopPropagation = True, preventDefault = True }
        (Json.Decode.succeed msg)


{-| <https://github.com/elm-lang/html/issues/136>
-}
label_ : String -> Html.Attribute msg
label_ s =
    Hattr.property "label" (Json.Encode.string s)


badge : List (Html msg) -> Html msg
badge =
    Html.span [ class "badge" ]


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
