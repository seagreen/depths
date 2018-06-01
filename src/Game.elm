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
import Game.Type.Commands exposing (Commands)
import Game.Type.Geology as Geology exposing (Geology(..))
import Game.Type.Habitat as Habitat exposing (Habitat)
import Game.Type.Id as Id exposing (Id(..), IdSeed(..))
import Game.Type.Player exposing (Player(..))
import Game.Type.Tile exposing (Tile)
import Game.Type.Turn exposing (Turn(..), unTurn)
import Game.Type.Unit as Unit exposing (Submarine(..), Unit)
import HexGrid exposing (HexGrid(..), Point)
import Random.Pcg as Random
import State


resolveTurn : Commands -> Game -> ( List BattleReport, Game )
resolveTurn commands game =
    case outcome game of
        Victory _ ->
            ( [], game )

        Draw ->
            ( [], game )

        Ongoing ->
            game
                |> nameHabitats commands.habitatNamings
                >> resolveBuildOrders commands.buildOrders
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



--------------------------------------------------
-- Subfunctions of `resolveTurn`.
--------------------------------------------------


nameHabitats : Dict Int Habitat.Name -> Game -> Game
nameHabitats namings beforeGame =
    let
        name : Int -> Habitat.Name -> Game -> Game
        name habId newName game =
            let
                (HexGrid a oldGrid) =
                    game.grid

                newGrid : Dict Point Tile
                newGrid =
                    Game.State.updateHabitatById
                        (Id habId)
                        (\hab -> { hab | name = Just newName })
                        oldGrid
            in
            { game | grid = HexGrid a newGrid }
    in
    Dict.foldr name beforeGame namings


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
            State.traverseDict
                (resolveSingleBattle game.turn)
                oldGrid
                ( game.randomSeed, [] )
    in
    ( battleReports
    , { game
        | grid = HexGrid a newGrid
        , randomSeed = newRandomSeed
      }
    )


resolveSingleBattle :
    Turn
    -> Tile
    -> ( Random.Seed, List BattleReport )
    -> ( Tile, ( Random.Seed, List BattleReport ) )
resolveSingleBattle turn tile ( seed0, reports0 ) =
    case tile.fixed of
        Depths ->
            ( tile, ( seed0, reports0 ) )

        Mountain Nothing ->
            ( tile, ( seed0, reports0 ) )

        Mountain (Just hab) ->
            let
                ( defenders, attackers ) =
                    List.partition
                        (\unit -> unit.player == hab.player)
                        (Dict.values tile.units)

                resolve :
                    Tile
                    -> Habitat
                    -> Random.Seed
                    -> ( ( List BattleEvent, Tile ), Random.Seed )
                resolve tile hab =
                    let
                        defendingCombatants : List Combatant
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
                    ( tile, ( seed0, reports0 ) )

                _ ->
                    let
                        ( ( newEvents, newTile ), seed1 ) =
                            resolve tile hab seed0
                    in
                    ( newTile
                    , ( seed1
                      , { turn = turn
                        , habitat = hab
                        , events = newEvents
                        }
                            :: reports0
                      )
                    )


twoSidedBattle :
    List Unit
    -> List Combatant
    -> Habitat
    -> Tile
    -> Random.Seed
    -> ( ( List BattleEvent, Tile ), Random.Seed )
twoSidedBattle attackingUnits defenders hab tile seed0 =
    let
        attackers : List Combatant
        attackers =
            List.map CMUnit attackingUnits

        f :
            List Combatant
            -> List Combatant
            -> Random.Seed
            -> ( List BattleEvent, Random.Seed )
        f firing targets seed0_ =
            let
                ( detections, seed1_ ) =
                    detectedInCombat firing targets seed0_

                ( destroyed, seed2_ ) =
                    destroyedInCombat
                        firing
                        (List.map .detected detections)
                        seed1_
            in
            ( List.map DestructionEvent destroyed
                ++ List.map DetectionEvent detections
            , seed2_
            )

        ( battleEventsA, seed1 ) =
            f attackers defenders seed0

        ( battleEventsB, seed2 ) =
            f defenders attackers seed1

        battleEvents : List BattleEvent
        battleEvents =
            battleEventsA ++ battleEventsB

        destructionEvents : List Destruction
        destructionEvents =
            List.filterMap
                (\e ->
                    case e of
                        DestructionEvent a ->
                            Just a

                        DetectionEvent _ ->
                            Nothing
                )
                battleEvents
    in
    ( ( battleEvents, removeDestroyed destructionEvents tile )
    , seed2
    )


{-| When there aren't any defenders at all:

  - Non-combat buildings can be targeted.
  - These buildings are automatically detected.

-}
bombard :
    List Unit
    -> Habitat
    -> Tile
    -> Random.Seed
    -> ( ( List BattleEvent, Tile ), Random.Seed )
bombard attackingUnits hab tile seed0 =
    let
        ( destroyed, seed1 ) =
            destroyedInCombat attackers buildings seed0

        battleEvents : List BattleEvent
        battleEvents =
            List.map DestructionEvent destroyed

        attackers : List Combatant
        attackers =
            List.map CMUnit attackingUnits

        -- HACK: Converting non-Combatant buildings to Combatants
        buildings : List Combatant
        buildings =
            List.map (CMBuilding hab.player) hab.buildings

        destructionEvents : List Destruction
        destructionEvents =
            List.filterMap
                (\e ->
                    case e of
                        DestructionEvent a ->
                            Just a

                        DetectionEvent _ ->
                            Nothing
                )
                battleEvents
    in
    ( ( battleEvents, removeDestroyed destructionEvents tile )
    , seed1
    )


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
            State.traverseDict resolveProduction oldGrid game.nextUnitId
    in
    { game
        | grid = HexGrid a newGrid
        , nextUnitId = newIdSeed
    }


resolveProduction : Tile -> IdSeed -> ( Tile, IdSeed )
resolveProduction tile seed =
    case tile.fixed of
        Mountain (Just hab) ->
            let
                newProduced =
                    hab.produced + Building.production hab.buildings
            in
            case hab.producing of
                Nothing ->
                    ( tile, seed )

                Just producing ->
                    if newProduced < Buildable.cost producing then
                        let
                            newHabitat =
                                { hab | produced = newProduced }
                        in
                        ( { tile | fixed = Mountain (Just newHabitat) }, seed )
                    else
                        case producing of
                            BuildSubmarine sub ->
                                completeSubmarine sub tile hab seed

                            BuildBuilding building ->
                                ( completeBuilding building tile hab, seed )

        _ ->
            ( tile, seed )


completeSubmarine : Submarine -> Tile -> Habitat -> IdSeed -> ( Tile, IdSeed )
completeSubmarine sub tile hab seed0 =
    let
        ( id, seed1 ) =
            Id.next seed0

        newHabitat : Habitat
        newHabitat =
            { hab
                | producing = Nothing
                , produced = 0
            }

        newUnit : Unit
        newUnit =
            { id = id
            , player = hab.player
            , class = sub
            }

        newTile : Tile
        newTile =
            { tile
                | fixed = Mountain (Just newHabitat)
                , units =
                    Dict.insert
                        (Id.unId id)
                        newUnit
                        tile.units
            }
    in
    ( newTile, seed1 )


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
