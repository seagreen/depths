module View.TechTable exposing (..)

import Game.Type.Building as Building exposing (Building)
import Game.Type.Unit as Unit exposing (Submarine)
import Html exposing (Html)
import Html.Attributes as Hattr


view : Html msg
view =
    Html.div
        []
        [ Html.h2
            []
            [ Html.text "Submarines" ]
        , Html.table
            [ Hattr.class "table table-bordered table-striped"
            ]
            [ Html.thead []
                [ Html.tr
                    []
                    [ Html.th
                        []
                        [ Html.text "Name" ]
                    , Html.th
                        []
                        [ Html.text "Cost" ]
                    , Html.th
                        []
                        [ Html.text "Prerequisites" ]
                    , Html.th
                        []
                        [ Html.text "Sensors" ]
                    , Html.th
                        []
                        [ Html.text "Firepower" ]
                    , Html.th
                        []
                        [ Html.text "Stealth" ]
                    , Html.th
                        []
                        [ Html.text "Move" ]
                    ]
                ]
            , Html.tbody
                []
                (List.map viewSubmarine Unit.all)
            ]
        , Html.h2
            []
            [ Html.text "Buildings" ]
        , Html.table
            [ Hattr.class "table table-bordered table-striped"
            ]
            [ Html.thead []
                [ Html.tr
                    []
                    [ Html.th
                        []
                        [ Html.text "Name" ]
                    , Html.th
                        []
                        [ Html.text "Cost" ]
                    , Html.th
                        []
                        [ Html.text "Prerequisites" ]
                    , Html.th
                        []
                        [ Html.text "Sensors" ]
                    , Html.th
                        []
                        [ Html.text "Firepower" ]
                    , Html.th
                        []
                        [ Html.text "Production bonus" ]
                    ]
                ]
            , Html.tbody
                []
                (List.map viewBuilding Building.all)
            ]
        ]


viewSubmarine : Submarine -> Html msg
viewSubmarine submarine =
    let
        stats =
            Unit.stats submarine
    in
    Html.tr
        []
        [ Html.td
            []
            [ Html.text stats.name ]
        , Html.td
            []
            [ Html.text (toString stats.cost) ]
        , Html.td
            []
            [ Html.text (String.join ", " (List.map toString stats.prerequisites)) ]
        , Html.td
            []
            [ Html.text (toString stats.sensors) ]
        , Html.td
            []
            [ Html.text (toString stats.firepower) ]
        , Html.td
            []
            [ Html.text (toString stats.stealth) ]
        , Html.td
            []
            [ Html.text (toString stats.speed) ]
        ]


viewBuilding : Building -> Html msg
viewBuilding building =
    let
        stats =
            Building.stats building

        ( sensors, firepower ) =
            case stats.combatStats of
                Just { sensors, firepower } ->
                    ( toString sensors, toString firepower )

                Nothing ->
                    ( "", "" )
    in
    Html.tr
        []
        [ Html.td
            []
            [ Html.text stats.name ]
        , Html.td
            []
            [ Html.text (toString stats.cost) ]
        , Html.td
            []
            [ Html.text (String.join ", " (List.map toString stats.prerequisites)) ]
        , Html.td
            []
            [ Html.text sensors ]
        , Html.td
            []
            [ Html.text firepower ]
        , Html.td
            []
            [ Html.text (showIfNonZero stats.productionBonus) ]
        ]


showIfNonZero : Int -> String
showIfNonZero i =
    case i of
        0 ->
            ""

        _ ->
            toString i
