module Main exposing (..)

import Html
import Keyboard
import Model
import Update
import View
import WebSocket


enter : Int
enter =
    13


main : Program { seed : Int } Model.Model Update.Msg
main =
    Html.programWithFlags
        { init = \{ seed } -> ( Model.init seed, initCommands )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


initCommands : Cmd Update.Msg
initCommands =
    Cmd.batch
        -- [ Model.newRandomSeed TODO
        []


subscriptions : Model.Model -> Sub Update.Msg
subscriptions model =
    let
        keydown : Sub Update.Msg
        keydown =
            Keyboard.downs
                (\keyPress ->
                    if keyPress == enter then
                        Update.EndRound
                    else
                        Update.NoOp
                )

        listen : Sub Update.Msg
        listen =
            WebSocket.listen model.server.url Update.Recv
    in
    case model.gameStatus of
        Model.NotPlayingYet ->
            Sub.none

        Model.WaitingForStart ->
            Sub.batch
                [ listen
                ]

        Model.InGame ->
            Sub.batch
                [ keydown
                , listen
                ]
