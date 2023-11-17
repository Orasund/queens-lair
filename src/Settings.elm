module Settings exposing (..)


type alias Settings =
    { withLives : Bool
    , withChests : Bool
    }


classic : Settings
classic =
    { withChests = True
    , withLives = False
    }


arcade : Settings
arcade =
    { classic | withChests = False }


modern : Settings
modern =
    { classic | withLives = True }
