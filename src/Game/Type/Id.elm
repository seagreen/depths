module Game.Type.Id exposing (..)

{-| Incrementally increasing IDs.
-}


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


next : IdSeed -> ( Id, IdSeed )
next (IdSeed n) =
    ( Id n, IdSeed (n + 1) )
