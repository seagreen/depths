module Game exposing (..)

{-| Contains `resolveTurn`, the game's core function.

Also contains the types to support `resolveTurn` (such as `BattleReport`)
that aren't part of the game state itself.

Types that are part of the game's state are imported from `Game.State`.

-}

import Dict exposing (Dict)
import Game.Building as Building exposing (Building(..))
import Game.Id as Id exposing (Id(..), IdSeed(..))
import Game.State
    exposing
        ( Buildable(..)
        , Game
        , Geology(..)
        , Habitat
        , HabitatEditor(..)
        , HabitatName
        , Tile
        , Turn(..)
        )
import Game.Unit as Unit exposing (Player(..), Submarine(..), Unit)
import HexGrid exposing (HexGrid(..), Point)
import Random
import Random.List
import State exposing (State(..))
import Util


resolveTurn : Commands -> Game -> ( List BattleReport, Game )
resolveTurn commands game =
    case outcome game of
        Just _ ->
            ( [], game )

        Nothing ->
            game
                |> resolveBuildOrders commands.buildOrders
                >> resolveMoves commands.moves
                >> resolveBattles
                >> Tuple.mapSecond
                    (destroyHabitats
                        >> wrapResolveProduction
                        >> wrapNewEnemies
                        >> incrementTurn
                    )



--------------------------------------------------
-- Supporting types
--------------------------------------------------


type Outcome
    = Victory
    | Defeat


outcome : Game -> Maybe Outcome
outcome game =
    if Game.State.unTurn game.turn >= 200 then
        Just Victory
    else
        let
            habitats =
                Game.State.habitatDict game.grid
        in
        if Dict.size habitats >= 4 then
            Just Victory
        else if
            Dict.isEmpty habitats
                && List.isEmpty (Game.State.friendlyUnitList (Util.unHexGrid game.grid))
        then
            Just Defeat
        else
            Nothing


{-| The keys in `moves` are the IDs of units.

At some point we should add the restriction that units can't
"move" to their current tile.

-}
type alias Commands =
    { moves : Dict Int Point
    , buildOrders : Dict Point Buildable
    }


type alias BattleReport =
    { turn : Turn
    , habitat : String
    , events : List BattleEvent
    }


type
    BattleEvent
    -- When the human player detects a computer unit or building.
    -- The first argument is the thing detected, the second is the
    -- detector.
    = DetectionEvent Buildable Buildable
      -- When either the human player or the computer destorys an enemy.
      -- The `Player` is the owner of the destroyed unit.
      -- The first unit is the one that was destroyed, the second
      -- (if known) is the destroyer.
    | DestructionEvent Player Buildable (Maybe Buildable)


type Combatant
    = CMUnit Unit
    | CMBuilding Building


buildableFromCombatant : Combatant -> Buildable
buildableFromCombatant combatant =
    case combatant of
        CMUnit unit ->
            BuildSubmarine unit.class

        CMBuilding building ->
            BuildBuilding building


combatantSensors : Combatant -> Int
combatantSensors combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).sensors

        CMBuilding building ->
            Building.sensors building


combatantStealth : Combatant -> Int
combatantStealth combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).stealth

        CMBuilding _ ->
            0


combatantFirepower : Combatant -> Int
combatantFirepower combatant =
    case combatant of
        CMUnit unit ->
            (Unit.stats unit.class).firepower

        CMBuilding building ->
            Building.firepower building



--------------------------------------------------
-- Subfunctions of `resolveTurn`.
--------------------------------------------------


incrementTurn : Game -> Game
incrementTurn game =
    { game | turn = Turn (Game.State.unTurn game.turn + 1) }


destroyHabitats : Game -> Game
destroyHabitats game =
    let
        (HexGrid a grid) =
            game.grid

        habitats =
            Game.State.habitatDict game.grid

        remove mTile =
            mTile |> Maybe.andThen (\tile -> Just { tile | fixed = Mountain Nothing })
    in
    { game
        | grid =
            HexGrid a <|
                Dict.foldr
                    (\point hab acc ->
                        if Building.population hab.buildings > 0 then
                            acc
                        else
                            Dict.update point remove acc
                    )
                    grid
                    habitats
    }


resolveBuildOrders : Dict Point Buildable -> Game -> Game
resolveBuildOrders orders game =
    Dict.foldr singleBuildOrder game orders


singleBuildOrder : Point -> Buildable -> Game -> Game
singleBuildOrder point buildable game =
    let
        setProduction hab =
            if Just buildable == hab.producing then
                hab
            else
                { hab
                    | producing = Just buildable
                    , produced = 0
                }

        (HexGrid a grid) =
            game.grid
    in
    { game
        | grid =
            HexGrid a (Game.State.updateHabitat point setProduction grid)
    }


resolveMoves : Dict Int Point -> Game -> Game
resolveMoves moves game =
    let
        (HexGrid a grid) =
            game.grid
    in
    { game | grid = HexGrid a <| Dict.foldr (resolveSingleMove << Id) grid moves }


resolveSingleMove : Id -> Point -> Dict Point Tile -> Dict Point Tile
resolveSingleMove id newPoint grid =
    case Game.State.findUnit id grid of
        Nothing ->
            grid

        Just ( oldPoint, unit ) ->
            if HexGrid.distance oldPoint newPoint <= (Unit.stats unit.class).speed then
                moveUnit oldPoint unit newPoint grid
            else
                grid


moveUnit : Point -> Unit -> Point -> Dict Point Tile -> Dict Point Tile
moveUnit oldPoint unit newPoint grid =
    let
        moveTo tile =
            { tile
                | units =
                    Dict.insert (Id.unId unit.id) unit tile.units
            }

        newTile tile =
            Just <|
                case tile.fixed of
                    Mountain Nothing ->
                        case unit.class of
                            ColonySubmarine ->
                                { tile | fixed = Mountain (Just (Game.State.newHabitat unit.id)) }

                            _ ->
                                moveTo tile

                    _ ->
                        moveTo tile
    in
    grid
        |> Dict.update oldPoint (Maybe.map (removeUnit unit.id))
        |> Dict.update newPoint (Maybe.andThen newTile)


wrapResolveProduction : Game -> Game
wrapResolveProduction game =
    let
        (HexGrid a oldGrid) =
            game.grid

        ( newGrid, newIdSeed ) =
            State.run game.idSeed <|
                Util.traverseStateDict resolveProduction oldGrid
    in
    { game
        | grid = HexGrid a newGrid
        , idSeed = newIdSeed
    }


resolveProduction : Tile -> State IdSeed Tile
resolveProduction tile =
    case tile.fixed of
        Mountain (Just hab) ->
            let
                newProduced =
                    hab.produced + Building.production hab.buildings
            in
            case hab.producing of
                Nothing ->
                    State.state tile

                Just producing ->
                    if newProduced < Game.State.cost producing then
                        let
                            newHabitat =
                                { hab | produced = newProduced }
                        in
                        State.state { tile | fixed = Mountain (Just newHabitat) }
                    else
                        case producing of
                            BuildSubmarine sub ->
                                completeSubmarine sub tile hab

                            BuildBuilding building ->
                                State.state <| completeBuilding building tile hab

        _ ->
            State.state tile


completeSubmarine : Submarine -> Tile -> Habitat -> State IdSeed Tile
completeSubmarine sub tile hab =
    let
        newHabitat =
            { hab
                | producing = Nothing
                , produced = 0
            }

        newUnit id =
            { id = id
            , player = Human
            , class = sub
            }

        complete id =
            { tile
                | fixed = Mountain (Just newHabitat)
                , units =
                    Dict.insert
                        (Id.unId id)
                        (newUnit id)
                        tile.units
            }
    in
    State.map complete Id.next


completeBuilding : Building -> Tile -> Habitat -> Tile
completeBuilding building tile hab =
    let
        newHabitat =
            { hab
                | buildings = building :: hab.buildings
                , producing = Nothing
                , produced = 0
            }
    in
    { tile | fixed = Mountain (Just newHabitat) }


removeUnit : Id -> Tile -> Tile
removeUnit id tile =
    { tile | units = Dict.remove (Id.unId id) tile.units }


resolveBattles : Game -> ( List BattleReport, Game )
resolveBattles game =
    let
        (HexGrid a oldGrid) =
            game.grid

        ( newGrid, ( newRandomSeed, battleReports ) ) =
            State.run ( game.randomSeed, [] ) <|
                Util.traverseStateDict (resolveSingleBattle game.turn) oldGrid
    in
    ( battleReports
    , { game
        | grid = HexGrid a newGrid
        , randomSeed = newRandomSeed
      }
    )


{-| `List Buildable` is a list of the ships and buildings that scored sensor hits.
-}
countSensorHits : List Combatant -> State Random.Seed (List Buildable)
countSensorHits searcherList =
    let
        rollSensors : Combatant -> State Random.Seed (Maybe Buildable)
        rollSensors searcher =
            State <|
                Random.step
                    (Random.map
                        (\n ->
                            if combatantSensors searcher >= n then
                                Just (buildableFromCombatant searcher)
                            else
                                Nothing
                        )
                        (Random.int 1 6)
                    )
    in
    State.map
        (List.filterMap identity)
        (State.traverse rollSensors searcherList)


type alias Detected =
    { dtTarget : Combatant
    , dtBy : Buildable
    }


type alias Destroyed =
    { dsTarget : Combatant
    , dsBy : Buildable
    }


{-| `List Buildable` are the ships and buildings that scored sensor hits.
-}
detectedCombatants : List Combatant -> List Buildable -> State Random.Seed (List Detected)
detectedCombatants lurkers hitList =
    State <|
        Random.step
            (Random.map2
                (List.map2
                    (\buildable detected ->
                        { dtTarget = detected
                        , dtBy = buildable
                        }
                    )
                )
                (Random.List.shuffle hitList)
                (Random.List.shuffle lurkers)
            )


{-| A successful stealth roll makes a unit immune to detection.
-}
canceledByStealth : List Detected -> State Random.Seed (List Detected)
canceledByStealth detectedList =
    let
        rollStealth : Detected -> State Random.Seed (Maybe Detected)
        rollStealth detected =
            State <|
                Random.step
                    (Random.map
                        (\n ->
                            if combatantStealth detected.dtTarget >= n then
                                Nothing
                            else
                                Just detected
                        )
                        (Random.int 1 6)
                    )
    in
    State.map
        (List.filterMap identity)
        (State.traverse rollStealth detectedList)


{-| `List Buildable` are the ships and buildings that scored weapon hits.
-}
countFirepowerHits : List Combatant -> State Random.Seed (List Buildable)
countFirepowerHits firing =
    let
        rollFirepower : Combatant -> State Random.Seed (Maybe Buildable)
        rollFirepower combatant =
            State <|
                Random.step
                    (Random.map
                        (\n ->
                            if combatantFirepower combatant >= n then
                                Just (buildableFromCombatant combatant)
                            else
                                Nothing
                        )
                        (Random.int 1 4)
                    )
    in
    State.map
        (List.filterMap identity)
        (State.traverse rollFirepower firing)


{-| `List Buildable` are the ships and buildings that scored weapon hits.
-}
destroyedByFirepower : List Combatant -> List Buildable -> State Random.Seed (List Destroyed)
destroyedByFirepower targets hitList =
    State <|
        Random.step <|
            Random.map2
                (List.map2
                    (\buildable destroyed ->
                        { dsTarget = destroyed
                        , dsBy = buildable
                        }
                    )
                )
                (Random.List.shuffle hitList)
                (Random.List.shuffle targets)


enemiesDetected :
    List Combatant
    -> List Unit
    -> State Random.Seed ( List BattleEvent, List Combatant )
enemiesDetected defenders enemies =
    let
        reportEvent : Detected -> BattleEvent
        reportEvent detection =
            DetectionEvent
                (buildableFromCombatant detection.dtTarget)
                detection.dtBy

        report : List Detected -> ( List BattleEvent, List Combatant )
        report detectedUnits =
            ( List.map reportEvent detectedUnits
            , List.map .dtTarget detectedUnits
            )
    in
    State <|
        \oldSeed ->
            State.run oldSeed
                (countSensorHits defenders
                    |> State.andThen (detectedCombatants (List.map CMUnit enemies))
                    |> State.andThen canceledByStealth
                    |> State.map report
                )


enemiesDestroyed :
    List Combatant
    -> List Combatant
    -> Tile
    -> State Random.Seed ( List BattleEvent, Tile )
enemiesDestroyed defenders enemies tile =
    let
        reportEvent : Destroyed -> BattleEvent
        reportEvent destruction =
            DestructionEvent
                Computer
                (buildableFromCombatant destruction.dsTarget)
                (Just destruction.dsBy)
    in
    State <|
        \oldSeed ->
            State.run oldSeed
                (countFirepowerHits defenders
                    |> State.andThen (destroyedByFirepower enemies)
                    |> State.map
                        (\destroyed ->
                            ( List.map reportEvent destroyed
                            , removeDestroyed destroyed Nothing tile
                            )
                        )
                )


{-| If the caller doesn't have access to the `Habitat`
-- (such as when calculating the attacker's losses),
-- it can pass in `Nothing` to skip removing buildings.
-}
removeDestroyed : List Destroyed -> Maybe Habitat -> Tile -> Tile
removeDestroyed destroyed mHab tile =
    { tile
        | units = removeUnits tile.units destroyed
        , fixed =
            case mHab of
                Nothing ->
                    tile.fixed

                Just hab ->
                    Mountain (Just (removeBuildings hab destroyed))
    }


removeUnits : Dict Int Unit -> List Destroyed -> Dict Int Unit
removeUnits starting toRemove =
    Dict.diff
        starting
        (Dict.fromList <|
            List.filterMap
                (\destruction ->
                    case destruction.dsTarget of
                        CMUnit unit ->
                            Just
                                ( Id.unId unit.id
                                , unit
                                )

                        _ ->
                            Nothing
                )
                toRemove
        )


removeBuildings : Habitat -> List Destroyed -> Habitat
removeBuildings hab destroyed =
    let
        destroyedBuildings =
            List.filterMap
                (\destroyed ->
                    case destroyed.dsTarget of
                        CMBuilding building ->
                            Just building

                        _ ->
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


friendliesDestroyed :
    Habitat
    -> List Combatant
    -> List Unit
    -> Tile
    -> State Random.Seed ( List BattleEvent, Tile )
friendliesDestroyed hab defenders enemies tile =
    let
        reportEvent : Destroyed -> BattleEvent
        reportEvent destruction =
            DestructionEvent
                Human
                (buildableFromCombatant destruction.dsTarget)
                (Just destruction.dsBy)
    in
    State <|
        \oldSeed ->
            State.run oldSeed
                (countSensorHits (List.map CMUnit enemies)
                    |> State.andThen (detectedCombatants defenders)
                    |> State.andThen canceledByStealth
                    |> State.andThen
                        (\defendersDetected ->
                            countFirepowerHits (List.map CMUnit enemies)
                                |> State.andThen
                                    (destroyedByFirepower
                                        (List.map .dtTarget defendersDetected)
                                    )
                                |> State.map
                                    (\destroyed ->
                                        ( List.map reportEvent destroyed
                                        , removeDestroyed destroyed (Just hab) tile
                                        )
                                    )
                        )
                )


{-| When there aren't any defenders at all:

  - Non-combat buildings can be targeted.
  - These buildings are automatically detected.

-}
bombard : List Unit -> Habitat -> Tile -> State Random.Seed ( List BattleEvent, Tile )
bombard firing hab tile =
    let
        reportEvent : Destroyed -> BattleEvent
        reportEvent destruction =
            DestructionEvent
                Human
                (buildableFromCombatant destruction.dsTarget)
                (Just destruction.dsBy)

        reportAndRemove : List Destroyed -> ( List BattleEvent, Tile )
        reportAndRemove destroyed =
            ( List.map reportEvent destroyed
            , { tile
                | units = removeUnits tile.units destroyed
                , fixed = Mountain (Just (removeBuildings hab destroyed))
              }
            )
    in
    State <|
        \oldSeed ->
            State.run oldSeed
                (countFirepowerHits (List.map CMUnit firing)
                    |> State.andThen
                        (destroyedByFirepower (List.map CMBuilding hab.buildings))
                    |> State.map reportAndRemove
                )


twoSidedBattle :
    List Combatant
    -> List Unit
    -> Habitat
    -> Tile
    -> State Random.Seed ( List BattleEvent, Tile )
twoSidedBattle defenders enemies hab tile =
    enemiesDetected defenders enemies
        |> State.andThen
            (\( dtEvents, detected ) ->
                enemiesDestroyed defenders detected tile
                    |> State.andThen
                        (\( dsEvents, nextTile ) ->
                            friendliesDestroyed hab defenders enemies nextTile
                                |> State.map
                                    (Tuple.mapFirst
                                        (\friendlyDsEvents ->
                                            friendlyDsEvents
                                                ++ dsEvents
                                                ++ dtEvents
                                        )
                                    )
                        )
            )


resolveSingleBattle : Turn -> Tile -> State ( Random.Seed, List BattleReport ) Tile
resolveSingleBattle turn tile =
    let
        ( friendlyUnits, enemies ) =
            List.partition
                (\unit -> unit.player == Human)
                (Dict.values tile.units)

        resolve : Tile -> Habitat -> State Random.Seed ( List BattleEvent, Tile )
        resolve tile hab =
            let
                defenders =
                    List.map CMBuilding (Building.combatBuildings hab.buildings)
                        ++ List.map CMUnit friendlyUnits
            in
            case defenders of
                [] ->
                    bombard enemies hab tile

                _ ->
                    twoSidedBattle defenders enemies hab tile
    in
    case tile.fixed of
        Mountain (Just hab) ->
            case enemies of
                [] ->
                    State.state tile

                _ ->
                    State
                        (\( oldSeed, reports ) ->
                            let
                                ( ( events, newTile ), newSeed ) =
                                    State.run oldSeed (resolve tile hab)
                            in
                            case events of
                                [] ->
                                    ( tile, ( newSeed, reports ) )

                                nonEmpty ->
                                    ( newTile
                                    , ( newSeed
                                      , { turn = turn
                                        , habitat = Game.State.habitatFullName hab
                                        , events = nonEmpty
                                        }
                                            :: reports
                                      )
                                    )
                        )

        _ ->
            State.state tile


wrapNewEnemies : Game -> Game
wrapNewEnemies game =
    let
        (HexGrid a oldGrid) =
            game.grid

        ( newGrid, ( newRandomSeed, newIdSeed ) ) =
            State.run ( game.randomSeed, game.idSeed ) <|
                -- TODO: Don't pass both game and grid.
                newEnemies game oldGrid
    in
    { game
        | grid = HexGrid a newGrid
        , randomSeed = newRandomSeed
        , idSeed = newIdSeed
    }


newEnemies : Game -> Dict Point Tile -> State ( Random.Seed, IdSeed ) (Dict Point Tile)
newEnemies game dict =
    let
        placeEnemies point idA idB idC =
            let
                enemy id =
                    Unit id Computer AttackSubmarine

                smallEnemy id =
                    Unit id Computer RemotelyOperatedVehicle
            in
            Dict.update
                point
                (Maybe.andThen
                    (\tile ->
                        Just
                            { tile
                                | units =
                                    Dict.insert (Id.unId idA) (enemy idA) tile.units
                                        |> Dict.insert (Id.unId idB) (enemy idB)
                                        |> Dict.insert (Id.unId idC) (smallEnemy idC)
                            }
                    )
                )
                dict
    in
    -- TODO: Place enemies at random times; make them increasingly challenging.
    if
        Game.State.unTurn game.turn
            > 25
            && Game.State.unTurn game.turn
            % 20
            == 0
    then
        let
            pointsAndHabs =
                Dict.toList (Game.State.habitatDict game.grid)
        in
        State <|
            \( randomSeed, idSeed ) ->
                let
                    ( shuffledPoints, newRandomSeed ) =
                        -- PERFORMANCE: Don't need to shuffle the whole list.
                        -- Get a `select` function added to random-extra.
                        Random.step (Random.List.shuffle pointsAndHabs) randomSeed
                in
                case shuffledPoints of
                    ( point, _ ) :: _ ->
                        let
                            ( newDict, newIdSeed ) =
                                State.run idSeed <|
                                    State.map3 (placeEnemies point) Id.next Id.next Id.next
                        in
                        ( newDict, ( newRandomSeed, newIdSeed ) )

                    _ ->
                        ( dict, ( newRandomSeed, idSeed ) )
    else
        State.state dict
