module View.BuildMenu exposing (buildMenu, statsView)

import Game.Type.Buildable as Buildable exposing (Buildable(..))
import Game.Type.Building as Building exposing (Building(..))
import Game.Type.Habitat exposing (Habitat)
import Game.Type.Unit as Unit exposing (Submarine(..))
import HexGrid exposing (Point)
import Html as H exposing (Html)
import Html.Attributes as Hattr exposing (class)
import Html.Events as Hevent
import Model exposing (Model)
import Update exposing (Msg(..))
import View.Board exposing (getRemainingProduction)


buildMenu : Model -> Point -> Habitat -> Html Msg
buildMenu model point hab =
    H.div [ class "c-build-menu" ]
        [ H.header [ class "c-build-menu__header" ] [ H.text "Build Menu" ]
        , currentJob model point hab
        , warnAbandon hab
        , viewBuildChoices hab.buildings
        ]


warnAbandon : Habitat -> Html Msg
warnAbandon hab =
    case hab.producing of
        Just buildable ->
            H.div [ class "alert alert-warning c-build-menu__warning" ]
                [ H.text
                    ("Warning!: Queueing up a build will abandon and destroy your unfinished "
                        ++ Buildable.name buildable
                    )
                ]

        Nothing ->
            H.text ""


{-| view the currently producing job, if any
-}
currentJob : Model -> Point -> Habitat -> Html Msg
currentJob model point habitat =
    -- TODO: Fix this, should check BuildOrder
    case habitat.producing of
        Nothing ->
            H.text ""

        Just buildable ->
            let
                productionRemaining =
                    getRemainingProduction model point habitat
                        |> Maybe.withDefault 0
                        |> toString

                productionRemainingView =
                    H.span [ class "c-build-menu__current-job__remaining" ]
                        [ H.text ("Remaining: " ++ productionRemaining) ]
            in
            H.div [ class "c-build-menu__current-job" ]
                [ productionRemainingView, buildItemView False buildable ]


{-| view all buildables at a given mountain
-}
viewBuildChoices : List Building -> Html Msg
viewBuildChoices currentBuildings =
    let
        buildableUnits =
            Unit.buildable currentBuildings |> List.map BuildSubmarine

        buildableBuildings =
            Building.buildable currentBuildings |> List.map BuildBuilding

        toItems =
            List.map (buildItemView True)

        unitsSection =
            if List.isEmpty buildableUnits then
                []
            else
                [ H.header [] [ H.text "Units: " ]
                , H.div [ class "c-build-menu__build-group" ] (toItems buildableUnits)
                ]

        buildingsSection =
            if List.isEmpty buildableBuildings then
                []
            else
                [ H.header [] [ H.text "Buildings: " ]
                , H.div [ class "c-build-menu__build-group" ] (toItems buildableBuildings)
                ]
    in
    H.div [ class "c-build-menu__buildables" ] (unitsSection ++ buildingsSection)


{-| if `isClickable`, clicking the icon will queue a new BuildOrder
-}
buildItemView : Bool -> Buildable -> Html Msg
buildItemView isClickable buildable =
    let
        attrs =
            [ Hattr.classList
                [ ( "c-build-menu__item", True )
                , ( "c-build-menu__item--clickable", isClickable )
                , ( "btn btn-light", isClickable )
                ]
            ]
                -- conditionally add the BuildOrder click handler
                ++ (if isClickable then
                        [ Hevent.onClick (BuildOrder buildable) ]
                    else
                        []
                   )

        name =
            Buildable.name buildable
    in
    H.div attrs
        [ H.div [ class "tooltip-container" ]
            [ H.text
                (if isClickable then
                    name
                 else
                    name ++ " in progress"
                )
            , buildableToolTip buildable
            ]
        ]


type alias StatTuple =
    ( String, Int )


buildableToolTip : Buildable -> Html Msg
buildableToolTip buildable =
    let
        statStrs : List StatTuple
        statStrs =
            case buildable of
                BuildSubmarine sub ->
                    let
                        { cost, speed, sensors, stealth, firepower } =
                            Unit.stats sub
                    in
                    [ ( "Cost", cost )
                    , ( "Speed", speed )
                    , ( "Sensors", sensors )
                    , ( "Stealth", stealth )
                    , ( "Firepower", firepower )
                    ]

                BuildBuilding building ->
                    let
                        { cost, productionBonus, combatStats } =
                            Building.stats building

                        combatStatStrs =
                            case combatStats of
                                Nothing ->
                                    []

                                Just { sensors, firepower } ->
                                    [ ( "Sensors", sensors )
                                    , ( "Firepower", firepower )
                                    ]
                    in
                    [ ( "Cost", cost )
                    , ( "Production Bonus", productionBonus )
                    ]
                        ++ combatStatStrs

        statBadge : StatTuple -> Html Msg
        statBadge ( field, num ) =
            let
                fieldIcon : String
                fieldIcon =
                    case field of
                        "Cost" ->
                            "fab fa-bitcoin"

                        "Speed" ->
                            "fas fa-tachometer-alt"

                        "Sensors" ->
                            "fas fa-broadcast-tower"

                        "Stealth" ->
                            "fa fa-user-secret"

                        "Firepower" ->
                            "fas fa-fire"

                        "Production Bonus" ->
                            "fas fa-parachute-box"

                        _ ->
                            ""
            in
            H.span [ class "c-stat-badge" ]
                [ H.i [ class fieldIcon ] []
                , H.text (toString num)
                ]
    in
    H.span [ class "c-stats-tip tooltip" ] (List.map statBadge statStrs)


statsView : Buildable -> Html Msg
statsView buildable =
    let
        badgify : StatTuple -> Html Msg
        badgify ( field, num ) =
            H.span [ class "badge badge-secondary" ]
                [ H.text (field ++ " - " ++ toString num) ]

        statStrs : List StatTuple
        statStrs =
            case buildable of
                BuildSubmarine sub ->
                    let
                        { cost, speed, sensors, stealth, firepower } =
                            Unit.stats sub
                    in
                    [ ( "Cost", cost )
                    , ( "Speed", speed )
                    , ( "Sensors", sensors )
                    , ( "Stealth", stealth )
                    , ( "Firepower", firepower )
                    ]

                BuildBuilding building ->
                    let
                        { cost, productionBonus, combatStats } =
                            Building.stats building

                        combatStatStrs =
                            case combatStats of
                                Nothing ->
                                    []

                                Just { sensors, firepower } ->
                                    [ ( "Sensors", sensors )
                                    , ( "Firepower", firepower )
                                    ]
                    in
                    [ ( "Cost", cost )
                    , ( "Production Bonus", productionBonus )
                    ]
                        ++ combatStatStrs
    in
    H.div [ class "c-stats" ]
        (List.map badgify statStrs)
