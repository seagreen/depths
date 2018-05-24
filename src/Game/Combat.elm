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
import State exposing (State(..))


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
countSensorHits : List Combatant -> State Random.Seed (List Combatant)
countSensorHits searcherList =
    let
        rollSensors : Combatant -> State Random.Seed (Maybe Combatant)
        rollSensors searcher =
            State <|
                Random.step <|
                    Random.map
                        (\n ->
                            if n <= combatantSensors searcher then
                                Just searcher
                            else
                                Nothing
                        )
                        (Random.int 1 6)
    in
    State.map
        (List.filterMap identity)
        (State.traverse rollSensors searcherList)


{-| `List Buildable` are the ships and buildings that scored sensor hits.
-}
detectedCombatants : List Combatant -> List Combatant -> State Random.Seed (List Detection)
detectedCombatants finderList lurkerList =
    State <|
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
canceledByStealth : List Detection -> State Random.Seed (List Detection)
canceledByStealth detectionList =
    let
        rollStealth : Detection -> State Random.Seed (Maybe Detection)
        rollStealth detection =
            State <|
                Random.step <|
                    Random.map
                        (\n ->
                            if n <= combatantStealth detection.detected then
                                Nothing
                            else
                                Just detection
                        )
                        (Random.int 1 6)
    in
    State.map
        (List.filterMap identity)
        (State.traverse rollStealth detectionList)


{-| The returned list is the ships and buildings that scored weapon hits.
-}
countFirepowerHits : List Combatant -> State Random.Seed (List Combatant)
countFirepowerHits combatantList =
    let
        rollFirepower : Combatant -> State Random.Seed (Maybe Combatant)
        rollFirepower combatant =
            State <|
                Random.step <|
                    Random.map
                        (\n ->
                            if n <= combatantFirepower combatant then
                                Just combatant
                            else
                                Nothing
                        )
                        (Random.int 1 6)
    in
    State.map
        (List.filterMap identity)
        (State.traverse rollFirepower combatantList)


destroyedByFirepower : List Combatant -> List Combatant -> State Random.Seed (List Destruction)
destroyedByFirepower shooterHits targetList =
    State <|
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
    -> State Random.Seed (List Detection)
detectedInCombat searchers lurkers =
    State <|
        \oldSeed ->
            State.run oldSeed
                (countSensorHits searchers
                    |> State.andThen (\searcherHits -> detectedCombatants searcherHits lurkers)
                    |> State.andThen canceledByStealth
                )


destroyedInCombat :
    List Combatant
    -> List Combatant
    -> State Random.Seed (List Destruction)
destroyedInCombat shooters targets =
    State <|
        \oldSeed ->
            State.run oldSeed
                (countFirepowerHits shooters
                    |> State.andThen (\hits -> destroyedByFirepower hits targets)
                )


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
