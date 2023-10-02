module View.Spritesheet exposing (..)

import Config
import Html exposing (Attribute, Html)
import Pixel


loot attrs =
    toImage attrs ( 1, 0 )


whitePawn : List (Attribute msg) -> Html msg
whitePawn attrs =
    toImage attrs ( 2, 1 )


blackPawn : List (Attribute msg) -> Html msg
blackPawn attrs =
    toImage attrs ( 2, 0 )


whiteRook : List (Attribute msg) -> Html msg
whiteRook attrs =
    toImage attrs ( 3, 1 )


blackRook : List (Attribute msg) -> Html msg
blackRook attrs =
    toImage attrs ( 3, 0 )


whiteKnight : List (Attribute msg) -> Html msg
whiteKnight attrs =
    toImage attrs ( 4, 1 )


blackKnight : List (Attribute msg) -> Html msg
blackKnight attrs =
    toImage attrs ( 4, 0 )


whiteBishop : List (Attribute msg) -> Html msg
whiteBishop attrs =
    toImage attrs ( 5, 1 )


blackBishop : List (Attribute msg) -> Html msg
blackBishop attrs =
    toImage attrs ( 5, 0 )


whiteQueen : List (Attribute msg) -> Html msg
whiteQueen attrs =
    toImage attrs ( 6, 1 )


blackQueen : List (Attribute msg) -> Html msg
blackQueen attrs =
    toImage attrs ( 6, 0 )


whiteKing : List (Attribute msg) -> Html msg
whiteKing attrs =
    toImage attrs ( 7, 1 )


blackKing : List (Attribute msg) -> Html msg
blackKing attrs =
    toImage attrs ( 7, 0 )


toImage : List (Attribute msg) -> ( Int, Int ) -> Html msg
toImage attrs ( x, y ) =
    Pixel.spriteImage
        (Pixel.pixelated :: attrs)
        { url = "assets/spritesheet.png"
        , pos = ( x, y )
        , width = Config.sqaureSize
        , height = Config.sqaureSize
        , spriteHeight = 16
        , spriteWidth = 16
        , sheetColumns = 8
        , sheetRows = 2
        }
