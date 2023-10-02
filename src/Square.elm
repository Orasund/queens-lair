module Square exposing (..)

import Piece exposing (Piece(..))


type alias Square =
    { piece : Piece
    , isWhite : Bool
    }
