module Protocol exposing (Message(..), decodeMessage)

{-| Types and serialization functions used in the client-server protocol.
-}

import Dict
import Game exposing (Commands)
import Game.Building exposing (Building(..))
import Game.State exposing (Buildable(..))
import Game.Unit exposing (Submarine(..))
import HexGrid exposing (Point)
import Json.Decode as Decode exposing (Decoder)


--------------------------------------------------------------------------------
-- Message types


type Message
    = JoinMessage
        { name : String
        , room : String
        }
    | StartGameMessage
        { seed : Int
        }
    | TurnMessage
        { commands : Commands
        }



--------------------------------------------------------------------------------
-- Message decoders


decodeMessage : Decoder Message
decodeMessage =
    let
        decode : String -> Decoder Message
        decode kind =
            case kind of
                "join" ->
                    Decode.field "payload" decodeJoinMessage

                "start-game" ->
                    Decode.field "payload" decodeStartGameMessage

                "turn" ->
                    Decode.field "payload" decodeTurnMessage

                _ ->
                    Decode.fail ("Unknown message type: " ++ kind)
    in
    Decode.field "kind" Decode.string
        |> Decode.andThen decode


decodeJoinMessage : Decoder Message
decodeJoinMessage =
    Decode.map2
        (\name room -> JoinMessage { name = name, room = room })
        (Decode.field "name" Decode.string)
        (Decode.field "room" Decode.string)


decodeStartGameMessage : Decoder Message
decodeStartGameMessage =
    Decode.map
        (\seed -> StartGameMessage { seed = seed })
        (Decode.field "seed" Decode.int)


decodeTurnMessage : Decoder Message
decodeTurnMessage =
    Decode.map
        (\commands -> TurnMessage { commands = commands })
        (Decode.map2
            (\moves buildOrders ->
                { moves = Dict.fromList moves
                , buildOrders = Dict.fromList buildOrders
                }
            )
            (Decode.list (decodePair Decode.int decodePoint))
            (Decode.list (decodePair decodePoint decodeBuildable))
        )


decodeBuildable : Decoder Buildable
decodeBuildable =
    let
        decode : String -> Decoder Buildable
        decode kind =
            case kind of
                "building" ->
                    decodeBuilding
                        |> Decode.field "payload"
                        |> Decode.map BuildBuilding

                "submarine" ->
                    decodeSubmarine
                        |> Decode.field "payload"
                        |> Decode.map BuildSubmarine

                _ ->
                    Decode.fail ("Unknown buildable: " ++ kind)

        decodeBuilding : Decoder Building
        decodeBuilding =
            Decode.string
                |> Decode.andThen
                    (\kind ->
                        case kind of
                            "PrefabHabitat" ->
                                Decode.succeed PrefabHabitat

                            "Dormitory" ->
                                Decode.succeed Dormitory

                            "ShippingDock" ->
                                Decode.succeed ShippingDock

                            "Factory" ->
                                Decode.succeed Factory

                            "Armory" ->
                                Decode.succeed Armory

                            "SubmarinePen" ->
                                Decode.succeed SubmarinePen

                            "WarningBouys" ->
                                Decode.succeed WarningBouys

                            "SonarArray" ->
                                Decode.succeed SonarArray

                            "TorpedoTube" ->
                                Decode.succeed TorpedoTube

                            "Residences" ->
                                Decode.succeed Residences

                            "Datacenter" ->
                                Decode.succeed Datacenter

                            "Supercomputer" ->
                                Decode.succeed Supercomputer

                            _ ->
                                Decode.fail ("Unknown building: " ++ kind)
                    )

        decodeSubmarine : Decoder Submarine
        decodeSubmarine =
            Decode.string
                |> Decode.andThen
                    (\kind ->
                        case kind of
                            "ColonySubmarine" ->
                                Decode.succeed ColonySubmarine

                            "RemotelyOperatedVehicle" ->
                                Decode.succeed RemotelyOperatedVehicle

                            "AttackSubmarine" ->
                                Decode.succeed AttackSubmarine

                            _ ->
                                Decode.fail ("Unknown submarine: " ++ kind)
                    )
    in
    Decode.string
        |> Decode.andThen decode


decodePoint : Decoder Point
decodePoint =
    decodePair Decode.int Decode.int


decodePair : Decoder a -> Decoder b -> Decoder ( a, b )
decodePair =
    decodePairWith (\x y -> ( x, y ))


decodePairWith : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
decodePairWith f dx dy =
    Decode.map2
        f
        (Decode.index 0 dx)
        (Decode.index 1 dy)



--------------------------------------------------------------------------------
-- Message encoders
