module View.Level exposing (..)

import Action exposing (Movement(..))
import Config
import Dict
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import Piece exposing (Piece(..))
import Set
import View.Spritesheet
import View.Square


toHtml :
    { selected : Maybe ( Int, Int )
    , onSelect : Maybe ( Int, Int ) -> msg
    , movementOverride : Maybe Movement
    }
    -> Level
    -> Html msg
toHtml args game =
    let
        isValidMove move =
            case args.movementOverride of
                Just movement ->
                    case movement of
                        PieceMovement piece ->
                            { game
                                | board =
                                    game.board
                                        |> Dict.insert move.from
                                            { isWhite = True
                                            , piece = piece
                                            }
                            }
                                |> Level.isValidMove move

                        ToChest ->
                            Set.member move.to game.loot

                Nothing ->
                    game |> Level.isValidMove move
    in
    List.range 0 (Config.boardSize - 1)
        |> List.map
            (\y ->
                List.range 0 (Config.boardSize - 1)
                    |> List.map
                        (\x ->
                            let
                                isValid =
                                    isValidMove
                                        { from =
                                            args.selected
                                                |> Maybe.withDefault ( -1, -1 )
                                        , to = ( x, y )
                                        }
                            in
                            [ Layout.el
                                [ Html.Attributes.style "height" (String.fromInt (Config.sqaureSize - Config.sqaureSize // 8) ++ "px")
                                , Html.Attributes.style "width" (String.fromInt (Config.sqaureSize - Config.sqaureSize // 8) ++ "px")
                                , (if args.selected == Just ( x, y ) then
                                    "#fcff00"

                                   else if x + y |> modBy 2 |> (==) 0 then
                                    "var(--dark-gray-color)"

                                   else
                                    "var(--gray-color)"
                                  )
                                    |> Html.Attributes.style "background-color"
                                , (if isValid then
                                    "#fcff00"

                                   else
                                    "transparent"
                                  )
                                    |> (\c -> Html.Attributes.style "border" (String.fromInt (Config.sqaureSize // 16) ++ "px solid " ++ c))
                                ]
                                Layout.none
                            , case game.board |> Dict.get ( x, y ) of
                                Just square ->
                                    View.Square.toHtml
                                        [ Html.Attributes.style "position" "absolute"
                                        , Html.Attributes.style "top" "-20px"
                                        , Html.Attributes.style "left" "0"
                                        ]
                                        square

                                Nothing ->
                                    if Set.member ( x, y ) game.loot then
                                        View.Spritesheet.loot
                                            [ Html.Attributes.style "position" "absolute"
                                            , Html.Attributes.style "top" "-20px"
                                            , Html.Attributes.style "left" "0"
                                            ]

                                    else
                                        Layout.none
                            , Layout.el
                                ([ Html.Attributes.style "height" (String.fromInt Config.sqaureSize ++ "px")
                                 , Html.Attributes.style "width" (String.fromInt Config.sqaureSize ++ "px")
                                 , Html.Attributes.style "position" "absolute"
                                 , Html.Attributes.style "top" "0"
                                 , Html.Attributes.style "left" "0"
                                 , Html.Attributes.style "z-index" "1"
                                 ]
                                    ++ Layout.asButton
                                        { label = "Select " ++ String.fromInt x ++ "," ++ String.fromInt y
                                        , onPress =
                                            case args.selected of
                                                Just from ->
                                                    if ( x, y ) == from then
                                                        args.onSelect Nothing |> Just

                                                    else if isValidMove { from = from, to = ( x, y ) } then
                                                        args.onSelect (Just ( x, y )) |> Just

                                                    else
                                                        Nothing

                                                Nothing ->
                                                    if
                                                        game.board
                                                            |> Dict.get ( x, y )
                                                            |> Maybe.map .isWhite
                                                            |> Maybe.withDefault False
                                                    then
                                                        args.onSelect (Just ( x, y )) |> Just

                                                    else
                                                        Nothing
                                        }
                                )
                                Layout.none
                            ]
                                |> Html.div [ Html.Attributes.style "position" "relative" ]
                        )
                    |> Layout.row []
            )
        |> Layout.column []
