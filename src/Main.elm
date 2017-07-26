module Main exposing (..)

-- Core

import Keyboard


-- 3rd

import Html


-- Local

import Model
import Update
import View


main : Program Never Model.Model Model.Msg
main =
    Html.program
        { init = ( Model.init, Model.setRandomSeed )
        , update = \a b -> ( Update.update a b, Cmd.none )
        , view = View.view
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Keyboard.downs
                        (\keyPress ->
                            if
                                -- Enter key
                                keyPress == 13
                            then
                                Model.EndTurn
                            else
                                Model.NoOp
                        )
                    ]
        }
