module View exposing (view)

import Either exposing (Either(..))
import Game exposing (Outcome(..))
import Game.Building as Building exposing (Building(..))
import Game.Combat as Combat
    exposing
        ( BattleEvent(..)
        , BattleReport
        , Combatant(..)
        )
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
import Model exposing (GameType(..), Model, Selection(..))
import String
import Svg exposing (Svg)
import Svg.Events as Sevent exposing (onClick, onMouseOut, onMouseOver)
import Update exposing (Msg(..))
import View.Board as Board


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
                    ++ (case model.currentPlayer of
                            Player1 ->
                                "1"

                            Player2 ->
                                "2"
                                    ++ "'s turn"
                       )
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
                                Depths ->
                                    Html.text ""

                                Mountain Nothing ->
                                    Html.text ""

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
                        [ Board.viewBoard model ]
                    , endTurnButton model
                    ]
                ]
            ]
        ]


displayOutcome : Game -> Html Msg
displayOutcome game =
    case Game.outcome game of
        Victory player ->
            Html.div
                [ class "alert alert-success" ]
                [ Html.text <| "Glorious victory to " ++ toString player ++ "!" ]

        Draw ->
            Html.div
                [ class "alert alert-danger" ]
                [ Html.text "Draws are just losses on both sides." ]

        Ongoing ->
            Html.text ""


displayBattleReports : Model -> Html Msg
displayBattleReports model =
    let
        attackDescription : Combatant -> String
        attackDescription combatant =
            case combatant of
                CMUnit sub ->
                    case sub.class of
                        RemotelyOperatedVehicle ->
                            "a torpedo from a ROV."

                        AttackSubmarine ->
                            "a torpedo from an attack submarine."

                        _ ->
                            "submarine-based weapons."

                CMBuilding _ building ->
                    case building of
                        TorpedoTube ->
                            "habitat-launched torpedoes."

                        _ ->
                            "habitat-based weapons."

        displayEvent : BattleEvent -> Html Msg
        displayEvent event =
            let
                actor combatant =
                    if Combat.combatantPlayer combatant == model.currentPlayer then
                        "Our "
                    else
                        "Enemy "

                acted combatant =
                    if Combat.combatantPlayer combatant == model.currentPlayer then
                        "our "
                    else
                        "an enemy "
            in
            Html.li
                []
                [ case event of
                    DetectionEvent { detector, detected } ->
                        Html.text <|
                            actor detector
                                ++ Game.name (Combat.buildableFromCombatant detector)
                                ++ (case detected of
                                        CMUnit sub ->
                                            " detected "
                                                ++ acted detected
                                                ++ (Unit.stats sub.class).name

                                        CMBuilding _ enemyBuilding ->
                                            " found "
                                                ++ acted detected
                                                ++ (Building.stats enemyBuilding).name
                                   )
                                ++ "."

                    DestructionEvent { destroyer, destroyed } ->
                        Html.text <|
                            actor destroyer
                                ++ Game.name (Combat.buildableFromCombatant destroyed)
                                ++ " was destroyed by "
                                ++ acted destroyed
                                ++ attackDescription destroyer
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


viewHabitat : Model -> Point -> Habitat -> Html Msg
viewHabitat model point hab =
    let
        friendlyHabitat : Html Msg
        friendlyHabitat =
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

        enemyHabitat : Html Msg
        enemyHabitat =
            Html.div
                [ onClick (SelectTile point)
                , class "alert alert-danger"
                ]
                [ Html.h4
                    []
                    [ Html.b
                        []
                        [ Html.text <| Game.habitatFullName hab ]
                    ]
                ]
    in
    if hab.player == model.currentPlayer then
        friendlyHabitat
    else if Board.hasShipAtPoint model point then
        enemyHabitat
    else
        Html.text ""


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
                        ++ (case Board.getRemainingProduction model point hab of
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


endTurnButton : Model -> Html Msg
endTurnButton model =
    case Game.outcome model.game of
        Victory _ ->
            Html.text ""

        Draw ->
            Html.text ""

        Ongoing ->
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


{-| <https://github.com/elm-lang/html/issues/136>
-}
label_ : String -> Html.Attribute msg
label_ s =
    Hattr.property "label" (Json.Encode.string s)


badge : List (Html msg) -> Html msg
badge =
    Html.span [ class "badge" ]


{-| Generic onchange handler for <select>
-}
onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    Hevent.on "change" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string
