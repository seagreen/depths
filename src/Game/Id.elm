module Game.Id exposing (..)

{-| Incrementally increasing IDs.
-}

import State exposing (State(..))


type Id
    = Id Int


type IdSeed
    = IdSeed Int


unId : Id -> Int
unId (Id n) =
    n


unIdSeed : IdSeed -> Int
unIdSeed (IdSeed n) =
    n


next : State IdSeed Id
next =
    State (\(IdSeed n) -> ( Id n, IdSeed (n + 1) ))
