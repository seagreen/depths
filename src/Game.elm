module Game exposing (..)

{-| Contains `resolveTurn`, the game's core function.

Also contains the types to support `resolveTurn` (such as `BattleReport`)
that aren't part of the game state itself.

Types that are part of the game's state are imported from `Game.State`.

-}

import Dict exposing (Dict)
import Game.Combat exposing (..)
import Game.State exposing (Game)
import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Type.Building as Building exposing (Building(..))
import Game.Type.Geology as Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id as Id exposing (Id(..), IdSeed(..))
import Game.Type.Player exposing (Player(..))
import Game.Type.Tile exposing (Tile)
import Game.Type.Turn exposing (Turn(..), unTurn)
import Game.Type.Unit as Unit exposing (Submarine(..), Unit)
import HexGrid exposing (HexGrid(..), Point)
import Random.Pcg as Random
import State exposing (State(..))
import Util


resolveTurn : Commands -> Game -> ( List BattleReport, Game )
resolveTurn commands game =
    case outcome game of
        Victory _ ->
            ( [], game )

        Draw ->
            ( [], game )

        Ongoing ->
            game
                |> resolveBuildOrders commands.buildOrders
                >> resolveMoves commands.moves
                >> resolveBattles
                >> Tuple.mapSecond
                    (destroyHabitats
                        >> wrapResolveProduction
                        >> incrementTurn
                    )



--------------------------------------------------
-- Supporting types
--------------------------------------------------


type Outcome
    = Ongoing
    | Victory Player
    | Draw


outcome : Game -> Outcome
outcome game =
    if unTurn game.turn < 20 then
        --HACK:  This is bork
        Ongoing
    else
        case ( Game.State.habitatsForPlayer Player1 game, Game.State.habitatsForPlayer Player2 game ) of
            ( [], [] ) ->
                Draw

            ( _, [] ) ->
                Victory Player1

            ( [], _ ) ->
                Victory Player2

            _ ->
                Ongoing


{-| The keys in `moves` are the IDs of units.
-}
type alias Commands =
    { moves : Dict Int Point
    , buildOrders : Dict Point Buildable
    }



--------------------------------------------------
-- Subfunctions of `resolveTurn`.
--------------------------------------------------


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
    { game
        | grid = HexGrid a <| Dict.foldr (resolveSingleMove << Id) grid moves
    }


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
        removeFromLocation : Id -> Tile -> Tile
        removeFromLocation id tile =
            { tile | units = Dict.remove (Id.unId id) tile.units }

        moveTo : Tile -> Tile
        moveTo tile =
            { tile
                | units =
                    Dict.insert (Id.unId unit.id) unit tile.units
            }

        establishHabitat : Tile -> Tile
        establishHabitat tile =
            { tile | fixed = Mountain (Just (Habitat.new unit.player unit.id)) }

        newTile : Tile -> Tile
        newTile tile =
            case unit.class of
                ColonySub ->
                    case tile.fixed of
                        Depths ->
                            moveTo tile

                        Mountain (Just _) ->
                            moveTo tile

                        Mountain Nothing ->
                            establishHabitat tile

                _ ->
                    moveTo tile
    in
    grid
        |> Dict.update oldPoint (Maybe.map (removeFromLocation unit.id))
        |> Dict.update newPoint (Maybe.map newTile)


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


resolveSingleBattle : Turn -> Tile -> State ( Random.Seed, List BattleReport ) Tile
resolveSingleBattle turn tile =
    case tile.fixed of
        Depths ->
            State.state tile

        Mountain Nothing ->
            State.state tile

        Mountain (Just hab) ->
            let
                ( defenders, attackers ) =
                    List.partition
                        (\unit -> unit.player == hab.player)
                        (Dict.values tile.units)

                resolve : Tile -> Habitat -> State Random.Seed ( List BattleEvent, Tile )
                resolve tile hab =
                    let
                        defendingCombatants =
                            List.map (CMBuilding hab.player) (Building.combatBuildings hab.buildings)
                                ++ List.map CMUnit defenders
                    in
                    case defendingCombatants of
                        [] ->
                            bombard attackers hab tile

                        _ ->
                            twoSidedBattle attackers defendingCombatants hab tile
            in
            case attackers of
                [] ->
                    State.state tile

                _ ->
                    State <|
                        \( oldSeed, oldReports ) ->
                            let
                                ( ( newEvents, newTile ), newSeed ) =
                                    State.run oldSeed (resolve tile hab)

                                newReports : List BattleReport
                                newReports =
                                    case newEvents of
                                        [] ->
                                            oldReports

                                        _ ->
                                            { turn = turn
                                            , habitat = Habitat.fullNameWithDefault hab
                                            , events = newEvents
                                            }
                                                :: oldReports
                            in
                            ( newTile
                            , ( newSeed
                              , newReports
                              )
                            )


twoSidedBattle :
    List Unit
    -> List Combatant
    -> Habitat
    -> Tile
    -> State Random.Seed ( List BattleEvent, Tile )
twoSidedBattle attackingUnits defenders hab tile =
    let
        attackers : List Combatant
        attackers =
            List.map CMUnit attackingUnits

        f : List Combatant -> List Combatant -> State Random.Seed (List BattleEvent)
        f firing targets =
            detectedInCombat firing targets
                |> State.andThen
                    (\detections ->
                        State.map
                            (\destroyed ->
                                List.map DestructionEvent destroyed
                                    ++ List.map DetectionEvent detections
                            )
                            (destroyedInCombat
                                firing
                                (List.map .detected detections)
                            )
                    )

        events : State Random.Seed (List BattleEvent)
        events =
            f attackers defenders
                |> State.andThen
                    (\eventsA ->
                        State.map
                            (\eventsB -> eventsA ++ eventsB)
                            (f defenders attackers)
                    )

        destructionEvents : List BattleEvent -> List Destruction
        destructionEvents =
            List.filterMap
                (\e ->
                    case e of
                        DestructionEvent a ->
                            Just a

                        DetectionEvent _ ->
                            Nothing
                )

        g : List BattleEvent -> ( List BattleEvent, Tile )
        g battleEvents =
            ( battleEvents, removeDestroyed (destructionEvents battleEvents) tile )
    in
    State.map g events


{-| When there aren't any defenders at all:

  - Non-combat buildings can be targeted.
  - These buildings are automatically detected.

-}
bombard : List Unit -> Habitat -> Tile -> State Random.Seed ( List BattleEvent, Tile )
bombard attackingUnits hab tile =
    let
        attackers : List Combatant
        attackers =
            List.map CMUnit attackingUnits

        -- HACK: Converting non-Combatant buildings to Combatants
        buildings : List Combatant
        buildings =
            List.map (CMBuilding hab.player) hab.buildings

        events : State Random.Seed (List BattleEvent)
        events =
            State.map
                (\destroyed ->
                    List.map DestructionEvent destroyed
                )
                (destroyedInCombat
                    attackers
                    buildings
                )

        destructionEvents : List BattleEvent -> List Destruction
        destructionEvents =
            List.filterMap
                (\e ->
                    case e of
                        DestructionEvent a ->
                            Just a

                        DetectionEvent _ ->
                            Nothing
                )

        g : List BattleEvent -> ( List BattleEvent, Tile )
        g battleEvents =
            ( battleEvents, removeDestroyed (destructionEvents battleEvents) tile )
    in
    State.map g events


destroyHabitats : Game -> Game
destroyHabitats game =
    let
        (HexGrid a grid) =
            game.grid

        habitats : Dict Point Habitat
        habitats =
            Game.State.habitatDict grid

        remove : Maybe Tile -> Maybe Tile
        remove =
            Maybe.map
                (\tile -> { tile | fixed = Mountain Nothing })
    in
    { game
        | grid =
            HexGrid a <|
                Dict.foldr
                    (\point hab acc ->
                        case hab.buildings of
                            [] ->
                                Dict.update point remove acc

                            _ ->
                                acc
                    )
                    grid
                    habitats
    }


wrapResolveProduction : Game -> Game
wrapResolveProduction game =
    let
        (HexGrid a oldGrid) =
            game.grid

        ( newGrid, newIdSeed ) =
            State.run game.nextUnitId <|
                Util.traverseStateDict resolveProduction oldGrid
    in
    { game
        | grid = HexGrid a newGrid
        , nextUnitId = newIdSeed
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
                    if newProduced < Buildable.cost producing then
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
            , player = hab.player
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


incrementTurn : Game -> Game
incrementTurn game =
    { game | turn = Turn (unTurn game.turn + 1) }
