module Game.Combat exposing (..)

import Dict exposing (Dict)
import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Type.Building as Building exposing (Building(..))
import Game.Type.Geology as Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id as Id exposing (Id(..), IdSeed(..))
import Game.Type.Player exposing (Player(..))
import Game.Type.Tile exposing (Tile)
import Game.Type.Turn exposing (Turn(..))
import Game.Type.Unit as Unit exposing (Submarine(..), Unit)
import Random.Pcg as Random
import Random.Pcg.List as RandomList
import State


type alias BattleReport =
    { turn : Turn
    , habitat : Habitat
    , events : List BattleEvent
    }


type BattleEvent
    = DetectionEvent Detection
    | DestructionEvent Destruction


type alias Detection =
    { detector : Combatant
    , detected : Combatant
    }


type alias Destruction =
    { destroyer : Combatant
    , destroyed : Combatant
    }


type Combatant
    = CMUnit Unit
    | CMBuilding Player Building


buildableFromCombatant : Combatant -> Buildable
buildableFromCombatant combatant =
    case combatant of
        CMUnit unit ->
            BuildSubmarine unit.class

        CMBuilding _ building ->
            BuildBuilding building


combatantPlayer : Combatant -> Player
combatantPlayer combatant =
    case combatant of
        CMUnit unit ->
            unit.player

        CMBuilding player _ ->
            player


combatantSensors : Combatant -> Int
combatantSensors combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).sensors

        CMBuilding _ building ->
            Building.sensors building


combatantStealth : Combatant -> Int
combatantStealth combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).stealth

        CMBuilding _ _ ->
            0


combatantFirepower : Combatant -> Int
combatantFirepower combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).firepower

        CMBuilding _ building ->
            Building.firepower building


{-| Returns the Combatants that scored sensor hits.
-}
countSensorHits :
    List Combatant
    -> Random.Seed
    -> ( List Combatant, Random.Seed )
countSensorHits searcherList seed0 =
    let
        rollSensors :
            Combatant
            -> Random.Seed
            -> ( Maybe Combatant, Random.Seed )
        rollSensors searcher =
            Random.step <|
                Random.map
                    (\n ->
                        if n <= combatantSensors searcher then
                            Just searcher
                        else
                            Nothing
                    )
                    (Random.int 1 6)

        ( combatants, seed1 ) =
            State.traverse rollSensors searcherList seed0
    in
    ( List.filterMap identity combatants, seed1 )


{-| `List Buildable` are the ships and buildings that scored sensor hits.
-}
detectedCombatants :
    List Combatant
    -> List Combatant
    -> Random.Seed
    -> ( List Detection, Random.Seed )
detectedCombatants finderList lurkerList =
    Random.step <|
        Random.map2
            (List.map2
                (\finder lurker ->
                    { detector = finder
                    , detected = lurker
                    }
                )
            )
            (RandomList.shuffle finderList)
            (RandomList.shuffle lurkerList)


{-| A successful stealth roll makes a unit immune to detection.
-}
canceledByStealth :
    List Detection
    -> Random.Seed
    -> ( List Detection, Random.Seed )
canceledByStealth detectionList seed0 =
    let
        rollStealth :
            Detection
            -> Random.Seed
            -> ( Maybe Detection, Random.Seed )
        rollStealth detection =
            Random.step <|
                Random.map
                    (\n ->
                        if n <= combatantStealth detection.detected then
                            Nothing
                        else
                            Just detection
                    )
                    (Random.int 1 6)

        ( detection, seed1 ) =
            State.traverse rollStealth detectionList seed0
    in
    ( List.filterMap identity detection, seed1 )


{-| The returned list is the ships and buildings that scored weapon hits.
-}
countFirepowerHits :
    List Combatant
    -> Random.Seed
    -> ( List Combatant, Random.Seed )
countFirepowerHits combatantList seed0 =
    let
        rollFirepower :
            Combatant
            -> Random.Seed
            -> ( Maybe Combatant, Random.Seed )
        rollFirepower combatant =
            Random.step <|
                Random.map
                    (\n ->
                        if n <= combatantFirepower combatant then
                            Just combatant
                        else
                            Nothing
                    )
                    (Random.int 1 6)

        ( combatants, seed1 ) =
            State.traverse rollFirepower combatantList seed0
    in
    ( List.filterMap identity combatants, seed1 )


destroyedByFirepower :
    List Combatant
    -> List Combatant
    -> Random.Seed
    -> ( List Destruction, Random.Seed )
destroyedByFirepower shooterHits targetList =
    Random.step <|
        Random.map2
            (List.map2
                (\shooter target ->
                    { destroyer = shooter
                    , destroyed = target
                    }
                )
            )
            (RandomList.shuffle shooterHits)
            (RandomList.shuffle targetList)


{-| Takes the searches and the units being searched for.

Returns the units found.

-}
detectedInCombat :
    List Combatant
    -> List Combatant
    -> Random.Seed
    -> ( List Detection, Random.Seed )
detectedInCombat searchers lurkers seed0 =
    let
        ( searcherHits, seed1 ) =
            countSensorHits searchers seed0

        ( detected, seed2 ) =
            detectedCombatants searcherHits lurkers seed1
    in
    canceledByStealth detected seed2


destroyedInCombat :
    List Combatant
    -> List Combatant
    -> Random.Seed
    -> ( List Destruction, Random.Seed )
destroyedInCombat shooters targets seed0 =
    let
        ( hits, seed1 ) =
            countFirepowerHits shooters seed0
    in
    destroyedByFirepower hits targets seed1


removeDestroyed : List Destruction -> Tile -> Tile
removeDestroyed destroyed tile =
    case tile.fixed of
        Depths ->
            tile

        Mountain Nothing ->
            tile

        Mountain (Just hab) ->
            { tile
                | units = removeUnits tile.units destroyed
                , fixed = Mountain (Just (removeBuildings hab destroyed))
            }


removeUnits : Dict Int Unit -> List Destruction -> Dict Int Unit
removeUnits starting toRemove =
    Dict.diff
        starting
        (Dict.fromList <|
            List.filterMap
                (\destruction ->
                    case destruction.destroyed of
                        CMUnit unit ->
                            Just
                                ( Id.unId unit.id
                                , unit
                                )

                        CMBuilding _ _ ->
                            Nothing
                )
                toRemove
        )


removeBuildings : Habitat -> List Destruction -> Habitat
removeBuildings hab destroyed =
    let
        destroyedBuildings =
            List.filterMap
                (\destruction ->
                    case destruction.destroyed of
                        CMBuilding _ building ->
                            Just building

                        CMUnit _ ->
                            Nothing
                )
                destroyed
    in
    { hab
        | buildings =
            List.filter
                (\building ->
                    not (List.member building destroyedBuildings)
                )
                hab.buildings
    }
