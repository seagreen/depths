module Game.Type.Turn exposing (..)


type Turn
    = Turn Int


unTurn : Turn -> Int
unTurn (Turn n) =
    n
