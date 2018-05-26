module View exposing (view)

import Game exposing (Outcome(..))
import Game.Type.Turn exposing (Turn(..), unTurn)
import Html exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Model exposing (GameType(..), Model, Screen(..), Selection(..), TurnStatus(..))
import Update exposing (Msg(..))
import View.Board as Board
import View.Lobby as Lobby
import View.Sidebar as Sidebar
import View.TechTable as TechTable


view : Model -> Html Msg
view model =
    case model.crashed of
        Just crashMessage ->
            Html.text ("Crashed: " ++ crashMessage)

        Nothing ->
            case model.gameStatus of
                NotPlayingYet ->
                    Html.map SplashScreen (Lobby.lobby model.server)

                WaitingForStart ->
                    Html.map never Lobby.waitingForPlayer

                InGame ->
                    let
                        -- HACK: When you name a habitat it doesn't get into
                        -- the game State until both you and your opponent
                        -- have moved and the game rolls over to the next turn.
                        --
                        -- However, we want the new name to show up on your board
                        -- immediately.
                        --
                        -- So when we view the board we update its habitat names
                        -- with the ones you've set this turn.
                        modelWithUpdatedHabs : Model
                        modelWithUpdatedHabs =
                            { model
                                | game =
                                    Game.nameHabitats model.habitatNamings model.game
                            }
                    in
                    Html.div [] [ viewGame modelWithUpdatedHabs ]


viewGame : Model -> Html Msg
viewGame model =
    let
        viewTitle : Html msg
        viewTitle =
            Html.header [ class "c-title" ] [ Html.text "The Depths" ]

        viewUserGuideLink : Html msg
        viewUserGuideLink =
            Html.div []
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
                        [ class "btn btn-secondary", Hattr.type_ "button" ]
                        [ Html.text "Mechanics (on GitHub)" ]
                    ]
                ]

        viewGameInfo : Html Msg
        viewGameInfo =
            Html.div [ class "c-end-turn" ]
                [ Html.span [ class "c-end-turn__number" ]
                    [ Html.text <| "Turn " ++ toString (unTurn model.game.turn) ]
                , endTurnButton model
                ]
    in
    Html.main_ []
        [ viewTitle
        , viewGameInfo
        , Html.div [ class "c-docs" ]
            [ viewUserGuideLink, changeScreenButton model.screen ]
        , case model.screen of
            TechTable ->
                TechTable.view

            Board ->
                Html.div [ class "row" ]
                    [ Html.div [ class "col-lg-5" ] [ Sidebar.viewSidebar model ]
                    , Html.div [ class "col-lg-7" ] [ Board.viewBoard model ]
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
    Html.div []
        [ Html.button
            [ class "btn btn-secondary"
            , Hattr.type_ "button"
            , Hevent.onClick (ChangeScreen newScreen)
            ]
            [ Html.text newScreenTitle ]
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
                        , Hattr.disabled True
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
                        , class "btn btn-secondary btn-lg"
                        , Hattr.disabled True
                        ]
                        [ Html.text "(Waiting)" ]
