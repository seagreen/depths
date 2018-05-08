module Main exposing (..)

import Html
import Keyboard
import Model
import Task
import Update
import View
import WebSocket


enter : Int
enter =
    13


main : Program { seed : Int } Model.Model Model.Msg
main =
    Html.programWithFlags
        { init = \{ seed } -> ( Model.init seed, initCommands )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


initCommands : Cmd Model.Msg
initCommands =
    Cmd.batch
        -- [ Model.newRandomSeed
        [ Task.perform identity (Task.succeed Model.Connect)
        ]


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    let
        keydown : Sub Model.Msg
        keydown =
            Keyboard.downs
                (\keyPress ->
                    if keyPress == enter then
                        Model.EndRound
                    else
                        Model.NoOp
                )

        listen : Sub Model.Msg
        listen =
            WebSocket.listen model.server.url Model.Recv
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
