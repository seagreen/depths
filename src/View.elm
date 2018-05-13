module View exposing (view)

import Game exposing (Outcome(..))
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
import Model exposing (GameType(..), Model, Selection(..), TurnStatus(..))
import Update exposing (Msg(..))
import Util exposing (badge, label_, onChange)
import View.Board as Board
import View.Sidebar as Sidebar
import View.TechTree as TechTree


view : Model -> Html Msg
view model =
    let
        joinGame : Html Msg
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
    case model.gameStatus of
        NotPlayingYet ->
            joinGame

        WaitingForStart ->
            Html.div [] [ Html.text "Waiting for other player." ]

        InGame ->
            Html.div [] [ viewGame model ]


viewGame : Model -> Html Msg
viewGame model =
    let
        (HexGrid _ dict) =
            model.game.grid

        game = model.game

        viewTitle : Html msg
        viewTitle =
            Html.h1 [] [ Html.text "The Depths" ]

        viewPlayer : Html msg
        viewPlayer =
            Html.div
                []
                [ Html.text <| Unit.playerToString model.currentPlayer
                ]

        viewUserGuideLink : Html msg
        viewUserGuideLink =
            Html.p
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

        viewTurnNumber : Html msg
        viewTurnNumber =
            Html.p
                []
                [ Html.text "Turn "
                , badge
                    [ Html.text (toString (Game.unTurn model.game.turn)) ]
                ]
    in
    Html.div
        []
        [ viewTitle
        , viewPlayer
        , TechTree.view
        , Html.div [] [Html.text <| toString game.nextUnitId]
        , Html.div [] [Html.text <| toString game.randomSeed]
        , Html.div
            [ class "row" ]
            [ Html.div
                [ class "col-lg-5" ]
                [ viewUserGuideLink
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
                        [ Hevent.onClick EndRound
                        , Hattr.type_ "button"
                        , class "btn btn-warning btn-lg"
                        ]
                        [ Html.text "Loading" ]

                TurnInProgress ->
                    Html.button
                        [ Hevent.onClick EndRound
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
