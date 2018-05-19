module Main exposing (..)

import Html
import Keyboard
import Model
import Update
import View
import WebSocket


enterKey : Int
enterKey =
    13


main : Program Never Model.Model Update.Msg
main =
    Html.program
        { init = ( Model.init, Cmd.none )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model.Model -> Sub Update.Msg
subscriptions model =
    let
        keydown : Sub Update.Msg
        keydown =
            Keyboard.downs
                (\keyPress ->
                    if keyPress == enterKey then
                        Update.Enter
                    else
                        Update.NoOp
                )

        listen : Sub Update.Msg
        listen =
            WebSocket.listen model.server.url Update.Recv

        listenIfConnected : List (Sub Update.Msg)
        listenIfConnected =
            case model.gameStatus of
                Model.NotPlayingYet ->
                    []

                Model.WaitingForStart ->
                    [ listen ]

                Model.InGame ->
                    [ listen ]
    in
    Sub.batch (keydown :: listenIfConnected)
