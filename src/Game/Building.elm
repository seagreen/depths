module Game.Building exposing (..)


type alias Stats =
    { name : String
    , cost : Int
    , prerequisites : List Building
    , productionBonus : Int
    , combatStats : Maybe CombatStats
    }


type alias CombatStats =
    { sensors : Int
    , firepower : Int
    }


stats : Building -> Stats
stats infra =
    case infra of
        PrefabHabitat ->
            { name = "Prefab Habitat"
            , cost = 10
            , prerequisites = []
            , productionBonus = 1
            , combatStats = Nothing
            }

        Dock ->
            { name = "Dock"
            , cost = 5
            , prerequisites = []
            , productionBonus = 1
            , combatStats = Nothing
            }

        Minefield ->
            { name = "Minefield"
            , cost = 5
            , prerequisites = []
            , productionBonus = 0
            , combatStats = Just { sensors = 2, firepower = 2 }
            }

        Dormitory ->
            { name = "Dormitory"
            , cost = 5
            , prerequisites = [ Dock ]
            , productionBonus = 0
            , combatStats = Nothing
            }

        Armory ->
            { name = "Armory"
            , cost = 5
            , prerequisites = [ Dock ]
            , productionBonus = 0
            , combatStats = Nothing
            }

        SonarArray ->
            { name = "Sonar Array"
            , cost = 6
            , prerequisites = [ Armory ]
            , productionBonus = 0
            , combatStats = Just { sensors = 4, firepower = 0 }
            }

        TorpedoTube ->
            { name = "Torpedo Tube"
            , cost = 6
            , prerequisites = [ Armory ]
            , productionBonus = 0
            , combatStats = Just { sensors = 0, firepower = 4 }
            }

        Reactor ->
            { name = "Reactor"
            , cost = 15
            , prerequisites = [ Dock ]
            , productionBonus = 0
            , combatStats = Nothing
            }

        Factory ->
            { name = "Factory"
            , cost = 20
            , prerequisites = [ Dock ]
            , productionBonus = 1
            , combatStats = Nothing
            }

        Datacenter ->
            { name = "Datacenter"
            , cost = 20
            , prerequisites = [ Factory ]
            , productionBonus = 1
            , combatStats = Nothing
            }

        Supercomputer ->
            { name = "Supercomputer"
            , cost = 50
            , prerequisites = [ Datacenter ]
            , productionBonus = 2
            , combatStats = Nothing
            }


type Building
    = PrefabHabitat
    | Dock
    | Minefield
    | Dormitory
    | Armory
    | SonarArray
    | TorpedoTube
    | Reactor
    | Factory
    | Datacenter
    | Supercomputer


all : List Building
all =
    [ PrefabHabitat
    , Dock
    , Minefield
    , Dormitory
    , Armory
    , SonarArray
    , TorpedoTube
    , Reactor
    , Factory
    , Datacenter
    , Supercomputer
    ]


fromString : String -> Maybe Building
fromString s =
    case s of
        "PrefabHabitat" ->
            Just PrefabHabitat

        "Dock" ->
            Just Dock

        "Minefield" ->
            Just Minefield

        "Dormitory" ->
            Just Dormitory

        "Armory" ->
            Just Armory

        "SonarArray" ->
            Just SonarArray

        "TorpedoTube" ->
            Just TorpedoTube

        "Reactor" ->
            Just Reactor

        "Factory" ->
            Just Factory

        "Datacenter" ->
            Just Datacenter

        "Supercomputer" ->
            Just Supercomputer

        _ ->
            Nothing


hasPrerequisites : Building -> List Building -> Bool
hasPrerequisites proposedBuilding currentBuildings =
    List.all
        (\prereq -> List.member prereq currentBuildings)
        (stats proposedBuilding).prerequisites


buildable : List Building -> List Building
buildable currentBuildings =
    -- I really wish Buildings were comparable so we could use sets.
    List.filter
        (\building ->
            not (List.member building currentBuildings)
                && hasPrerequisites building currentBuildings
        )
        all


combatBuildings : List Building -> List Building
combatBuildings =
    List.filter (\building -> (stats building).combatStats /= Nothing)


sensors : Building -> Int
sensors building =
    (stats building).combatStats
        |> Maybe.map .sensors
        |> Maybe.withDefault 0


firepower : Building -> Int
firepower building =
    (stats building).combatStats
        |> Maybe.map .firepower
        |> Maybe.withDefault 0


production : List Building -> Int
production =
    List.sum << List.map (.productionBonus << stats)
