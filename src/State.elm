module State exposing (..)

import Dict exposing (Dict)


traverse : (a -> s -> ( b, s )) -> List a -> s -> ( List b, s )
traverse f xs s0 =
    case xs of
        [] ->
            ( [], s0 )

        y :: ys ->
            let
                ( z, s1 ) =
                    f y s0

                ( zs, s2 ) =
                    traverse f ys s1
            in
            ( z :: zs, s2 )

traverseDict :
    (a -> s -> ( b, s ))
    -> Dict comparable a
    -> s
    -> ( Dict comparable b, s )
traverseDict f dict s =
    let
        g :
            comparable
            -> a
            -> ( Dict comparable b, s )
            -> ( Dict comparable b, s )
        g k a ( acc, s0 ) =
            let
                ( b, s1 ) =
                    f a s0
            in
            ( Dict.insert k b acc, s1 )
    in
    Dict.foldr g ( Dict.empty, s ) dict
