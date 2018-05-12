module View.TechTree exposing (..)

import Html exposing (Html)
import Game.Building as Building exposing (Building)
import Game.Unit as Unit exposing (Submarine)


view : Html msg
view =
    Html.div [] <|
      (List.map viewSubmarine Unit.all ++ List.map viewBuilding Building.all)


viewSubmarine : Submarine -> Html msg
viewSubmarine submarine =
    case Unit.stats submarine of
        { name, prerequisites } ->
            Html.div []
                [ Html.text
                    (name
                        ++ " <-- "
                        ++ String.join ", "
                            (List.map toString prerequisites)
                    )
                ]


viewBuilding : Building -> Html msg
viewBuilding building =
    case Building.stats building of
        { name, prerequisites } ->
            Html.div []
                [ Html.text
                    (name
                        ++ " <-- "
                        ++ String.join ", "
                            (List.map toString prerequisites)
                    )
                ]
