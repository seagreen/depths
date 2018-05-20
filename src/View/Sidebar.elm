module View.Sidebar exposing (..)

{-| The boxes that appear on the side of the screen.

Notifications, help messages, unit descriptions, etc.

-}

import Dict
import Game exposing (Outcome(..))
import Game.Combat as Combat
    exposing
        ( BattleEvent(..)
        , BattleReport
        , Combatant(..)
        )
import Game.State as Game exposing (Game)
import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Type.Building as Building exposing (Building(..))
import Game.Type.Geology as Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id exposing (Id(..), unId)
import Game.Type.Player as Player exposing (Player(..))
import Game.Type.Turn exposing (Turn(..), unTurn)
import Game.Type.Unit as Unit exposing (Submarine(..), Unit)
import HexGrid exposing (HexGrid(..), Point)
import Html exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Model exposing (GameType(..), Model, Selection(..))
import String
import Update exposing (Msg(..))
import Util exposing (badge, label_, onChange)
import View.Board as Board


viewSidebar : Model -> Html Msg
viewSidebar model =
    Html.div
        []
        [ displayOutcome model.game
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
                                , case Dict.get (unId hab.id) model.habitatNameEditors of
                                    Nothing ->
                                        Html.text ""

                                    Just editor ->
                                        viewHabitatNameForm hab.id editor
                                ]
                    , Html.div
                        []
                        (List.map
                            (viewUnit model.selection)
                            (Game.friendlyUnits model.player tile)
                        )
                    ]
        , startingHelpMessage model
        ]


displayOutcome : Game -> Html Msg
displayOutcome game =
    case Game.outcome game of
        Victory player ->
            Html.div
                [ class "alert alert-success" ]
                [ Html.text <| "Glorious victory to " ++ Player.niceString player ++ "!" ]

        Draw ->
            Html.div
                [ class "alert alert-danger" ]
                [ Html.text "Draws are just losses on both sides." ]

        Ongoing ->
            Html.text ""


displayBattleReports : Model -> Html Msg
displayBattleReports model =
    let
        occuredLastTurn : BattleReport -> Bool
        occuredLastTurn entry =
            unTurn entry.turn == unTurn model.game.turn - 1
    in
    Html.div
        []
        (List.map (displayReport model)
            (List.filter occuredLastTurn model.gameLog)
        )


displayReport : Model -> BattleReport -> Html Msg
displayReport model report =
    let
        visibleEvents : List (Html Msg)
        visibleEvents =
            List.filterMap (displayEventIfVisible model) report.events
                -- Show events from oldest to newest.
                |> List.reverse
    in
    case visibleEvents of
        [] ->
            Html.text ""

        _ ->
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
                    visibleEvents
                ]


displayEventIfVisible : Model -> BattleEvent -> Maybe (Html Msg)
displayEventIfVisible model event =
    let
        attackDescription : Combatant -> String
        attackDescription combatant =
            case combatant of
                CMUnit sub ->
                    case sub.class of
                        DieselSub ->
                            "a torpedo."

                        AttackSub ->
                            "a torpedo from an attack submarine."

                        _ ->
                            "submarine-based weapons."

                CMBuilding _ building ->
                    case building of
                        TorpedoTube ->
                            "habitat-launched torpedoes."

                        _ ->
                            "habitat-based weapons."

        actor combatant =
            if Combat.combatantPlayer combatant == model.player then
                "Our "
            else
                "Enemy "

        acted combatant =
            if Combat.combatantPlayer combatant == model.player then
                "our "
            else
                "an enemy "
    in
    case event of
        DetectionEvent { detector, detected } ->
            if Combat.combatantPlayer detector == model.player then
                Just <|
                    Html.li
                        []
                        [ Html.text <|
                            "Our "
                                ++ Buildable.name (Combat.buildableFromCombatant detector)
                                ++ (case detected of
                                        CMUnit sub ->
                                            " detected an enemy "
                                                ++ (Unit.stats sub.class).name

                                        CMBuilding _ enemyBuilding ->
                                            " found an enemy "
                                                ++ (Building.stats enemyBuilding).name
                                   )
                                ++ "."
                        ]
            else
                Nothing

        DestructionEvent { destroyer, destroyed } ->
            if Combat.combatantPlayer destroyer == model.player then
                Just <|
                    Html.li
                        []
                        [ Html.text <|
                            "Our "
                                ++ Buildable.name (Combat.buildableFromCombatant destroyer)
                                ++ " destroyed an enemy unit or structure."
                        ]
            else if Combat.combatantPlayer destroyed == model.player then
                Just <|
                    Html.li
                        []
                        [ Html.text <|
                            "Our "
                                ++ Buildable.name (Combat.buildableFromCombatant destroyed)
                                ++ " was destroyed by enemy action."
                        ]
            else
                Debug.crash "DestructionEvent where we weren't involved at all."


viewHabitat : Model -> Point -> Habitat -> Html Msg
viewHabitat model point hab =
    let
        friendlyHabitat : Html Msg
        friendlyHabitat =
            Html.div
                [ Hevent.onClick (SelectTile point)
                , class "alert alert-success"
                ]
                [ Html.h4
                    []
                    [ Html.b
                        []
                        [ Html.text <| Habitat.fullNameWithDefault hab ]
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
                    [ Html.text <|
                        "Buildings: "
                            ++ (hab.buildings
                                    |> List.reverse
                                    |> List.map toString
                                    |> List.intersperse ", "
                                    |> String.concat
                               )
                    ]
                ]

        enemyHabitat : Html Msg
        enemyHabitat =
            Html.div
                [ Hevent.onClick (SelectTile point)
                , class "alert alert-warning"
                ]
                [ Html.h4
                    []
                    [ Html.b
                        []
                        [ Html.text <| "Location: " ++ Habitat.fullNameWithDefault hab ]
                    ]
                ]
    in
    if hab.player == model.player then
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
                [ Html.text <|
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


viewHabitatNameForm : Id -> Habitat.NameEditor -> Html Msg
viewHabitatNameForm habId (Habitat.NameEditor editor) =
    Html.div
        [ class "alert alert-warning" ]
        [ Html.form
            [ Hevent.onSubmit (NameEditorSubmit habId) ]
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
                    [ Html.text "Full name:" ]
                , Html.input
                    [ class "form-control"
                    , Hattr.type_ "text"
                    , Hattr.id "habitatName"
                    , Hevent.onInput (NameEditorFull habId)
                    , Hattr.value editor.full
                    ]
                    []
                ]
            , Html.div
                [ class "form-group" ]
                [ Html.label
                    [ Hattr.for "habitatAbbreviation" ]
                    [ Html.text "Abbreviation (1-3 letters):" ]
                , Html.input
                    [ class "form-control"
                    , Hattr.type_ "text"
                    , Hattr.maxlength 3
                    , Hattr.id "habitatAbbreviation"
                    , Hevent.onInput (NameEditorAbbreviation habId)
                    , Hattr.value editor.abbreviation
                    ]
                    []
                ]
            , Html.button
                [ Hattr.type_ "submit" ]
                [ Html.text "Found" ]
            ]
        ]


viewUnit : Maybe Selection -> Unit -> Html Msg
viewUnit selection unit =
    let
        stats =
            Unit.stats unit.class
    in
    Html.div
        [ Hevent.onClick (SelectUnit unit.id)
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


startingHelpMessage : Model -> Html Msg
startingHelpMessage model =
    if
        unTurn model.game.turn
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
