module Game.Building exposing (..)


type alias Stats =
    { name : String
    , cost : Int
    , prerequisites : List Building
    , productionBonus : Int
    , populationBonus : Int
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
            , populationBonus = 1
            , combatStats = Nothing
            }

        Dormitory ->
            { name = "Dormitory"
            , cost = 5
            , prerequisites = []
            , productionBonus = 0
            , populationBonus = 1
            , combatStats = Nothing
            }

        ShippingDock ->
            { name = "Shipping Dock"
            , cost = 5
            , prerequisites = []
            , productionBonus = 1
            , populationBonus = 0
            , combatStats = Nothing
            }

        Factory ->
            { name = "Factory"
            , cost = 10
            , prerequisites = [ ShippingDock ]
            , productionBonus = 1
            , populationBonus = 0
            , combatStats = Nothing
            }

        Armory ->
            { name = "Armory"
            , cost = 10
            , prerequisites = []
            , productionBonus = 0
            , populationBonus = 0
            , combatStats = Nothing
            }

        SubmarinePen ->
            { name = "Submarine Pen"
            , cost = 10
            , prerequisites = [ ShippingDock ]
            , productionBonus = 0
            , populationBonus = 0
            , combatStats = Nothing
            }

        WarningBouys ->
            { name = "Warning Bouys"
            , cost = 4
            , prerequisites = []
            , productionBonus = 0
            , populationBonus = 0
            , combatStats = Just { sensors = 2, firepower = 0 }
            }

        SonarArray ->
            { name = "Sonar Array"
            , cost = 4
            , prerequisites = [ Armory ]
            , productionBonus = 0
            , populationBonus = 0
            , combatStats = Just { sensors = 2, firepower = 0 }
            }

        TorpedoTube ->
            { name = "Torpedo Tube"
            , cost = 6
            , prerequisites = [ Armory ]
            , productionBonus = 0
            , populationBonus = 0
            , combatStats = Just { sensors = 0, firepower = 3 }
            }

        Residences ->
            { name = "Residences"
            , cost = 5
            , prerequisites = [ Dormitory ]
            , productionBonus = 0
            , populationBonus = 1
            , combatStats = Nothing
            }

        Datacenter ->
            { name = "Datacenter"
            , cost = 20
            , prerequisites = [ ShippingDock ]
            , productionBonus = 1
            , populationBonus = 0
            , combatStats = Nothing
            }

        Supercomputer ->
            { name = "Supercomputer"
            , cost = 50
            , prerequisites = [ Datacenter ]
            , productionBonus = 2
            , populationBonus = 0
            , combatStats = Nothing
            }


type Building
    = PrefabHabitat
    | Dormitory
    | ShippingDock
    | Factory
    | Armory
    | SubmarinePen
    | WarningBouys
    | SonarArray
    | TorpedoTube
    | Residences
    | Datacenter
    | Supercomputer


all : List Building
all =
    [ PrefabHabitat
    , Dormitory
    , ShippingDock
    , Factory
    , Armory
    , SubmarinePen
    , WarningBouys
    , SonarArray
    , TorpedoTube
    , Residences
    , Datacenter
    , Supercomputer
    ]


fromString : String -> Maybe Building
fromString s =
    case s of
        "PrefabHabitat" ->
            Just PrefabHabitat

        "Dormitory" ->
            Just Dormitory

        "ShippingDock" ->
            Just ShippingDock

        "Factory" ->
            Just Factory

        "Armory" ->
            Just Armory

        "SubmarinePen" ->
            Just SubmarinePen

        "WarningBouys" ->
            Just WarningBouys

        "SonarArray" ->
            Just SonarArray

        "TorpedoTube" ->
            Just TorpedoTube

        "Residences" ->
            Just Residences

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


population : List Building -> Int
population =
    List.sum << List.map (.populationBonus << stats)


production : List Building -> Int
production =
    List.sum << List.map (.productionBonus << stats)
