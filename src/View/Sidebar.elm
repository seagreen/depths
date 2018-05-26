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
import Game.Type.Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id exposing (Id(..), unId)
import Game.Type.Player as Player exposing (Player(..))
import Game.Type.Turn exposing (Turn(..), unTurn)
import Game.Type.Unit as Unit exposing (Submarine(..), Unit)
import HexGrid exposing (HexGrid(..), Point)
import Html exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Model exposing (GameType(..), Model, Selection(..), TurnStatus(..))
import String
import Update exposing (Msg(..))
import Util exposing (badge)
import View.Board as Board
import View.BuildMenu exposing (buildMenu, statsView)


viewSidebar : Model -> Html Msg
viewSidebar model =
    Html.div [ class "c-sidebar" ]
        [ displayOutcome model.game
        , displayBattleReports model
        , case Model.focus model of
            Nothing ->
                Html.text ""

            Just ( point, tile ) ->
                Html.div []
                    [ case tile.fixed of
                        Depths ->
                            Html.text ""

                        Mountain Nothing ->
                            Html.text ""

                        Mountain (Just hab) ->
                            -- can only build on mountains
                            Html.div []
                                [ viewHabitat model point hab
                                , case
                                    Dict.get (unId hab.id)
                                        model.habitatNameEditors
                                  of
                                    Nothing ->
                                        Html.text ""

                                    Just editor ->
                                        viewHabitatNameForm model.turnStatus hab.id editor
                                ]
                    , Html.div []
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
                [ class "c-outcome alert alert-success" ]
                [ Html.text <|
                    "Glorious victory to "
                        ++ Player.niceString player
                        ++ "!"
                ]

        Draw ->
            Html.div
                [ class "c-outcome alert alert-danger" ]
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
            report.events
                |> List.filterMap (displayEventIfVisible model)
                -- Show events from oldest to newest:
                |> List.reverse

        viewReport : List (Html Msg) -> Html Msg
        viewReport contents =
            Html.div [ class "c-battle-report alert alert-danger" ]
                (Html.header []
                    [ Html.text <|
                        "Combat log: "
                            ++ Habitat.fullNameWithDefault report.habitat
                    ]
                    :: contents
                )
    in
    case visibleEvents of
        -- If we didn't score any sensor or firepower hits,
        -- and our opponent didn't score any firepower hits:
        [] ->
            -- If we we on the defense we don't even get to know there were
            --  enemies here:
            if report.habitat.player == model.player then
                Html.text ""
            else
                -- If we were on offense we at least know we tried:
                viewReport
                    [ Html.text
                        "We searched the sea floor, but couldn't find any targets."
                    ]

        _ ->
            viewReport
                [ Html.ol [] visibleEvents ]


displayEventIfVisible : Model -> BattleEvent -> Maybe (Html Msg)
displayEventIfVisible model event =
    case event of
        DetectionEvent { detector, detected } ->
            if Combat.combatantPlayer detector == model.player then
                Just <|
                    Html.li []
                        [ Html.text <|
                            "Our "
                                ++ Buildable.name
                                    (Combat.buildableFromCombatant detector)
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
                                ++ Buildable.name
                                    (Combat.buildableFromCombatant destroyer)
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
                , class "c-habitat c-habitat--friendly"
                ]
                [ Html.header
                    [ class "c-habitat__name" ]
                    [ Html.text <| Habitat.fullNameWithDefault hab ]
                , buildMenu model point hab
                , Html.div [ class "c-habitat__production" ]
                    [ Html.text "Production: "
                    , badge
                        [ Html.text <|
                            toString (Building.production hab.buildings)
                        ]
                    ]
                , Html.div [ class "c-habitat__buildings" ]
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
                , class "c-habitat c-habitat--enemy"
                ]
                [ Html.header []
                    [ Html.text <|
                        "Enemy habitat: "
                            ++ Habitat.fullNameWithDefault hab
                    ]
                ]
    in
    if hab.player == model.player then
        friendlyHabitat
    else if Board.hasShipAtPoint model point then
        enemyHabitat
    else
        Html.text ""


viewHabitatNameForm : TurnStatus -> Id -> Habitat.NameEditor -> Html Msg
viewHabitatNameForm turnStatus habId (Habitat.NameEditor editor) =
    let
        submitButton : Html Msg
        submitButton =
            Html.button
                [ Hevent.onClick (NameEditorSubmit habId)
                , Hattr.type_ "submit"
                ]
                [ Html.text "Found" ]
    in
    Html.div
        [ class "c-habitat-name-form alert alert-info" ]
        [ Html.div
            []
            [ Html.header [] [ Html.text "Name Habitat" ]
            , Html.div [ class "form-group" ]
                [ Html.label [ Hattr.for "habitatName" ]
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
            , Html.div [ class "form-group" ]
                [ Html.label
                    [ Hattr.for "habitat-abbreviation" ]
                    [ Html.text "Abbreviation (1-3 letters):" ]
                , Html.input
                    [ class "form-control"
                    , Hattr.type_ "text"
                    , Hattr.maxlength 3
                    , Hattr.id "habitat-abbreviation"
                    , Hevent.onInput (NameEditorAbbreviation habId)
                    , Hattr.value editor.abbreviation
                    ]
                    []
                ]
            , case turnStatus of
                TurnLoading ->
                    submitButton

                TurnInProgress ->
                    submitButton

                TurnComplete ->
                    Html.text ""
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
        , Hattr.classList
            [ ( "c-sidebar__unit", True )
            , ( "alert alert-success", True )
            , ( "focused", Just (SelectedId unit.id) == selection )
            ]
        ]
        [ Html.header [] [ Html.text stats.name ]
        , Maybe.withDefault (Html.text "") (Unit.helpText unit.class)
        , statsView (BuildSubmarine unit.class)
        ]


startingHelpMessage : Model -> Html Msg
startingHelpMessage model =
    let
        isFirstTurn =
            unTurn model.game.turn == 1

        -- assume the first unit is selected
        unitIsSelected =
            case model.selection of
                Just (SelectedId _) ->
                    True

                _ ->
                    False
    in
    if isFirstTurn && not unitIsSelected then
        Html.div
            [ class "c-starting-help-message alert alert-info" ]
            [ Html.text "Click the 'CS' tile to select your first unit." ]
    else
        Html.text ""
