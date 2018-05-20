module View exposing (view)

import Game exposing (Outcome(..))
import Game.Type.Player as Player exposing (Player(..))
import Game.Type.Turn exposing (Turn(..), unTurn)
import Html exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Model exposing (GameType(..), Model, Screen(..), Selection(..), TurnStatus(..))
import Update exposing (Msg(..), SplashScreenMsg(..))
import Util exposing (badge, label_, onChange)
import View.Board as Board
import View.Sidebar as Sidebar
import View.TechTable as TechTable


view : Model -> Html Msg
view model =
    let
        joinGame : Html SplashScreenMsg
        joinGame =
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
    in
    case model.crashed of
        Just crashMessage ->
            Html.text ("Crashed: " ++ crashMessage)

        Nothing ->
            case model.gameStatus of
                NotPlayingYet ->
                    Html.map SplashScreen joinGame

                WaitingForStart ->
                    Html.div [] [ Html.text "Waiting for other player." ]

                InGame ->
                    Html.div [] [ viewGame model ]


viewGame : Model -> Html Msg
viewGame model =
    let
        game =
            model.game

        viewTitle : Html msg
        viewTitle =
            Html.h1 [] [ Html.text "The Depths" ]

        viewPlayer : Html msg
        viewPlayer =
            Html.p
                []
                [ Html.text <| Player.niceString model.player
                ]

        viewUserGuideLink : Html msg
        viewUserGuideLink =
            Html.p
                []
                [ Html.a
                    [ Hattr.href "https://github.com/seagreen/fpg-depths#user-guide"

                    -- Open the link in a new tab. This is usually bad practice, but we do it here
                    -- because there isn't a way to reload a game once you leave.
                    , Hattr.target "_blank"
                    ]
                    -- Use label instead of button to prevent button from staying focused after
                    -- (a) right clicking it to open the link in a new window
                    -- or (b) clicking it and then hitting the back button.
                    --
                    -- Idea from: https://stackoverflow.com/a/34051869
                    [ Html.label
                        [ class "btn btn-default"
                        , Hattr.type_ "button"
                        ]
                        [ Html.text "Mechanics (on GitHub)" ]
                    ]
                ]

        viewTurnNumber : Html msg
        viewTurnNumber =
            Html.p
                []
                [ Html.text "Turn "
                , badge
                    [ Html.text (toString (unTurn model.game.turn)) ]
                ]
    in
    Html.div
        []
        [ viewTitle
        , viewUserGuideLink
        , changeScreenButton model.screen
        , case model.screen of
            TechTable ->
                TechTable.view

            Board ->
                Html.div
                    []
                    [ Html.div
                        [ class "row" ]
                        [ Html.div
                            [ class "col-lg-5" ]
                            [ viewPlayer
                            , viewTurnNumber
                            , Sidebar.viewSidebar model
                            ]
                        , Html.div
                            [ class "col-lg-7" ]
                            [ Html.div
                                [ class "text-center" ]
                                [ Board.viewBoard model
                                , endTurnButton model
                                ]
                            ]
                        ]
                    ]
        ]


changeScreenButton : Screen -> Html Msg
changeScreenButton screen =
    let
        ( newScreen, newScreenTitle ) =
            case screen of
                Board ->
                    ( TechTable, "Tech table" )

                TechTable ->
                    ( Board, "Board" )
    in
    Html.p []
        [ Html.label
            [ class "btn btn-default"
            , Hattr.type_ "button"
            , Hevent.onClick (ChangeScreen newScreen)
            ]
            [ Html.text newScreenTitle
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
            case model.turnStatus of
                TurnLoading ->
                    Html.button
                        [ Hattr.type_ "button"
                        , class "btn btn-warning btn-lg"
                        ]
                        [ Html.text "Loading" ]

                TurnInProgress ->
                    Html.button
                        [ Hevent.onClick EndTurnButton
                        , Hattr.type_ "button"
                        , class "btn btn-primary btn-lg"
                        ]
                        [ Html.text "End turn" ]

                TurnComplete ->
                    Html.button
                        [ Hattr.type_ "button"
                        , class "btn btn-default btn-lg"
                        ]
                        [ Html.text "(Waiting)" ]
