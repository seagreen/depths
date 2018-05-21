module View.Lobby exposing (lobby, waitingForPlayer)

import Html exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Protocol
import Update exposing (Msg(..), SplashScreenMsg(..))


{-| First screen in the lobby for choosing a server / room
-}
lobby : Protocol.Server -> Html SplashScreenMsg
lobby server =
    Html.main_ [ class "splash" ]
        [ Html.div [ class "c-join" ]
            [ Html.header []
                [ Html.span [] [ Html.text "Connect to " ]
                , Html.span [ class "c-join__title" ]
                    [ Html.text "The Depths" ]
                ]
            , Html.div [ class "form" ]
                [ Html.div [ class "form-group" ]
                    [ Html.label
                        [ Hattr.for "server"
                        , class "control-label"
                        ]
                        [ Html.text "Server Address" ]
                    , Html.input
                        [ Hattr.placeholder "server"
                        , Hattr.value server.url
                        , Hevent.onInput SetServerUrl
                        , Hattr.id "server"
                        , class "form-control"
                        ]
                        []
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.label
                        [ Hattr.for "room"
                        , class "control-label"
                        ]
                        [ Html.text "Room ID" ]
                    , Html.input
                        [ Hattr.placeholder "Room"
                        , Hattr.value server.room
                        , Hevent.onInput SetRoom
                        , Hattr.id "room"
                        , class "form-control"
                        ]
                        []
                    ]
                , Html.div [ class "form-group" ]
                    [ Html.button
                        [ Hevent.onClick Connect
                        , class "btn btn-primary c-join__connect"
                        ]
                        [ Html.text "Prepare to Dive" ]
                    ]
                ]
            ]
        , Html.div [ class "c-sub-splash" ]
            [ Html.img [ Hattr.src "./assets/sub1.svg" ] [] ]
        ]


{-| Waiting screen for when first joining
player is waiting for a partner
-}
waitingForPlayer : Html Never
waitingForPlayer =
    Html.main_ [ class "c-waiting-for-player" ]
        [ Html.div [ class "c-waiting-for-player__text" ]
            [ Html.text "Waiting for other player..." ]
        , Html.div [ Hattr.id "bubbles" ] []
        , Html.div [ class "bubble x1" ] []
        , Html.div [ class "bubble x2" ] []
        , Html.div [ class "bubble x3" ] []
        , Html.div [ class "bubble x4" ] []
        , Html.div [ class "bubble x5" ] []
        ]
