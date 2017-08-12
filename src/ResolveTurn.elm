module ResolveTurn exposing (..)

-- Core

import Dict exposing (Dict)
import Random


-- 3rd

import HexGrid exposing (HexGrid(..), Point)
import State exposing (State(..))
import Random.List


-- Local

import Game
    exposing
        ( Buildable(..)
        , Geology(..)
        , Habitat
        , HabitatEditor(..)
        , HabitatName
        , Tile
        , Turn(..)
        )
import Game.Building as Building exposing (Building(..))
import Game.Unit as Unit exposing (Unit, Player(..), Submarine(..))
import Game.Id as Id exposing (Id(..), IdSeed(..))
import Model
    exposing
        ( BattleEvent(..)
        , BattleReport
        , Combatant(..)
        , Model
        , Msg(..)
        , Outcome(..)
        , Selection(..)
        )
import Yaks


resolveTurn : Model -> Model
resolveTurn oldModel =
    case Model.outcome oldModel of
        Just _ ->
            oldModel

        Nothing ->
            let
                newModel =
                    oldModel
                        |> wrapResolveMoves
                        |> wrapResolveBattles
                        |> destroyHabitats
                        |> wrapResolveProduction
                        |> wrapNewEnemies
                        |> incrementTurn
            in
                { newModel | selection = newSelection newModel }


incrementTurn : Model -> Model
incrementTurn model =
    let
        oldGame =
            model.game
    in
        { model | game = { oldGame | turn = Turn (Game.unTurn model.game.turn + 1) } }


destroyHabitats : Model -> Model
destroyHabitats model =
    let
        (HexGrid a dict) =
            model.game.grid

        habitats =
            Game.habitatDict model.game.grid

        remove mTile =
            mTile |> Maybe.andThen (\tile -> Just { tile | fixed = Mountain Nothing })

        oldGame =
            model.game
    in
        { model
            | game =
                { oldGame
                    | grid =
                        HexGrid a <|
                            Dict.foldr
                                (\point hab acc ->
                                    if Building.population hab.buildings > 0 then
                                        acc
                                    else
                                        Dict.update point remove acc
                                )
                                dict
                                habitats
                }
        }


newSelection : Model -> Maybe Selection
newSelection model =
    let
        findUnit id =
            Model.findUnit id model.game.grid
                |> Maybe.andThen (\_ -> model.selection)

        maybeBecameHabitat id =
            Game.habitatDict model.game.grid
                |> Dict.toList
                |> (\habList ->
                        case List.filter (\( _, hab ) -> hab.createdBy == id) habList of
                            [ ( point, _ ) ] ->
                                Just (SelectedPoint point)

                            _ ->
                                Nothing
                   )
    in
        model.selection
            |> Maybe.andThen
                (\selection ->
                    case selection of
                        SelectedPoint _ ->
                            model.selection

                        SelectedId id ->
                            case findUnit id of
                                Nothing ->
                                    maybeBecameHabitat id

                                unit ->
                                    unit
                )


wrapResolveMoves : Model -> Model
wrapResolveMoves model =
    let
        (HexGrid a dict) =
            model.game.grid

        oldGame =
            model.game
    in
        { model | game = { oldGame | grid = HexGrid a (resolveMoves dict) } }


resolveMoves : Dict Point Tile -> Dict Point Tile
resolveMoves dict =
    Dict.foldr resolvePoint dict (Model.unitDict dict)


resolvePoint : Point -> Dict Int Unit -> Dict Point Tile -> Dict Point Tile
resolvePoint point units acc =
    Dict.foldr (\_ -> resolveUnit point) acc units


resolveUnit : Point -> Unit -> Dict Point Tile -> Dict Point Tile
resolveUnit point unit acc =
    case unit.plannedMove of
        Nothing ->
            acc

        Just new ->
            let
                standardMove tile =
                    { tile
                        | units =
                            Dict.insert (Id.unId unit.id)
                                { unit | plannedMove = Nothing }
                                tile.units
                    }

                newTile tile =
                    Just <|
                        case tile.fixed of
                            Mountain Nothing ->
                                case unit.class of
                                    ColonySubmarine ->
                                        { tile | fixed = Mountain (Just (Game.newHabitat unit.id)) }

                                    _ ->
                                        standardMove tile

                            Mountain _ ->
                                standardMove tile

                            Depths ->
                                standardMove tile
            in
                Dict.update new (Maybe.andThen newTile) <|
                    Dict.update point (Maybe.map (removeUnit unit.id)) acc


wrapResolveProduction : Model -> Model
wrapResolveProduction model =
    let
        (HexGrid a dict) =
            model.game.grid

        ( newDict, newIdSeed ) =
            State.run model.game.idSeed <|
                Yaks.traverseStateDict resolveProduction dict

        oldGame =
            model.game
    in
        { model
            | game =
                { oldGame
                    | grid = HexGrid a newDict
                    , idSeed = newIdSeed
                }
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
                        if newProduced < Game.cost producing then
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
            , plannedMove = Nothing
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


wrapResolveBattles : Model -> Model
wrapResolveBattles model =
    let
        (HexGrid a dict) =
            model.game.grid

        ( newDict, ( newRandomSeed, battleReports ) ) =
            State.run ( model.game.randomSeed, [] ) <|
                Yaks.traverseStateDict (resolveBattle model.game.turn) dict

        oldGame =
            model.game
    in
        { model
            | game =
                { oldGame
                    | grid = HexGrid a newDict
                    , randomSeed = newRandomSeed
                }
            , gameLog = battleReports ++ model.gameLog
        }


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
                            if Model.combatantSensors searcher >= n then
                                Just (Model.buildableFromCombatant searcher)
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
                            if Model.combatantStealth detected.dtTarget >= n then
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
                            if Model.combatantFirepower combatant >= n then
                                Just (Model.buildableFromCombatant combatant)
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
                (Model.buildableFromCombatant detection.dtTarget)
                detection.dtBy

        report : List Detected -> ( List BattleEvent, List Combatant )
        report detectedUnits =
            ( List.map reportEvent detectedUnits
            , List.map .dtTarget detectedUnits
            )
    in
        State <|
            (\oldSeed ->
                State.run oldSeed
                    (countSensorHits defenders
                        |> State.andThen (detectedCombatants (List.map CMUnit enemies))
                        |> State.andThen canceledByStealth
                        |> State.map report
                    )
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
                (Model.buildableFromCombatant destruction.dsTarget)
                (Just destruction.dsBy)
    in
        State <|
            (\oldSeed ->
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
                (Model.buildableFromCombatant destruction.dsTarget)
                (Just destruction.dsBy)
    in
        State <|
            (\oldSeed ->
                (State.run oldSeed
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
                )
            )


{-| When there aren't any defenders at all:
--
-- + Non-combat buildings can be targeted.
-- + These buildings are automatically detected.
-}
bombard : List Unit -> Habitat -> Tile -> State Random.Seed ( List BattleEvent, Tile )
bombard firing hab tile =
    let
        reportEvent : Destroyed -> BattleEvent
        reportEvent destruction =
            DestructionEvent
                Human
                (Model.buildableFromCombatant destruction.dsTarget)
                (Just (destruction.dsBy))

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
            (\oldSeed ->
                (State.run oldSeed
                    (countFirepowerHits (List.map CMUnit firing)
                        |> State.andThen
                            (destroyedByFirepower (List.map CMBuilding hab.buildings))
                        |> State.map reportAndRemove
                    )
                )
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


resolveBattle : Turn -> Tile -> State ( Random.Seed, List BattleReport ) Tile
resolveBattle turn tile =
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
                                                , habitat = Game.habitatFullName hab
                                                , events = nonEmpty
                                                }
                                                    :: reports
                                              )
                                            )
                            )

            _ ->
                State.state tile


wrapNewEnemies : Model -> Model
wrapNewEnemies model =
    let
        (HexGrid a dict) =
            model.game.grid

        ( newDict, ( newRandomSeed, newIdSeed ) ) =
            State.run ( model.game.randomSeed, model.game.idSeed ) <|
                newEnemies model dict

        oldGame =
            model.game
    in
        { model
            | game =
                { oldGame
                    | grid = HexGrid a newDict
                    , randomSeed = newRandomSeed
                    , idSeed = newIdSeed
                }
        }


newEnemies : Model -> Dict Point Tile -> State ( Random.Seed, IdSeed ) (Dict Point Tile)
newEnemies model dict =
    let
        placeEnemies point idA idB idC =
            let
                enemy id =
                    (Unit id Computer AttackSubmarine Nothing)

                smallEnemy id =
                    (Unit id Computer RemotelyOperatedVehicle Nothing)
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
        if Game.unTurn model.game.turn > 25 && Game.unTurn model.game.turn % 20 == 0 then
            let
                pointsAndHabs =
                    Dict.toList (Game.habitatDict model.game.grid)
            in
                State <|
                    (\( randomSeed, idSeed ) ->
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
                    )
        else
            State.state dict
