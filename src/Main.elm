module Main exposing (..)

import Html
import Keyboard
import Model
import Update
import View


enter : Int
enter =
    13


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = ( Model.init, Model.newRandomSeed )
        , update = \a b -> ( Update.update a b, Cmd.none )
        , view = View.view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Keyboard.downs
                        (\keyPress ->
                            if keyPress == enter then
                                Model.EndRound
                            else
                                Model.NoOp
                        )
                    ]
        }
