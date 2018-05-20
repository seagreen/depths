module Game.Type.Geology exposing (..)

import Game.Type.Habitat exposing (Habitat)


type Geology
    = Depths
    | Mountain (Maybe Habitat)
