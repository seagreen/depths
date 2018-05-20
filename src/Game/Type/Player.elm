module Game.Type.Player exposing (..)


type Player
    = Player1
    | Player2


niceString : Player -> String
niceString player =
    case player of
        Player1 ->
            "Player 1"

        Player2 ->
            "Player 2"
