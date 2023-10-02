module View.Square exposing (..)

import Html exposing (Attribute, Html)
import Piece exposing (Piece(..))
import Square exposing (Square)
import View.Spritesheet


toHtml : List (Attribute msg) -> Square -> Html msg
toHtml attrs square =
    case ( square.piece, square.isWhite ) of
        ( Pawn, True ) ->
            View.Spritesheet.whitePawn attrs

        ( Pawn, False ) ->
            View.Spritesheet.blackPawn attrs

        ( Rook, True ) ->
            View.Spritesheet.whiteRook attrs

        ( Rook, False ) ->
            View.Spritesheet.blackRook attrs

        ( Bishop, True ) ->
            View.Spritesheet.whiteBishop attrs

        ( Bishop, False ) ->
            View.Spritesheet.blackBishop attrs

        ( Knight, True ) ->
            View.Spritesheet.whiteKnight attrs

        ( Knight, False ) ->
            View.Spritesheet.blackKnight attrs

        ( Queen, True ) ->
            View.Spritesheet.whiteQueen attrs

        ( Queen, False ) ->
            View.Spritesheet.blackQueen attrs

        ( King, True ) ->
            View.Spritesheet.whiteKing attrs

        ( King, False ) ->
            View.Spritesheet.blackKing attrs
