module View.Overlay exposing (..)

import Action exposing (Action(..))
import Artefact exposing (Artefact)
import Config
import Html exposing (Html)
import Html.Attributes
import Layout
import Pixel
import View.Artefact
import View.Spritesheet


foundArtefact : { onCloseOverlay : Action -> msg, artefacts : List Artefact } -> Artefact -> Html msg
foundArtefact args artefact =
    [ "You have found the artefact "
        ++ Artefact.name artefact
        ++ "."
        |> Layout.text
            [ Html.Attributes.style "padding" "var(--space)"
            , Html.Attributes.style "background-color" "var(--dark-gray-color)"
            ]
    , [ View.Artefact.info artefact
      , if List.length args.artefacts < Config.maxArtefacts then
            Layout.textButton []
                { label = "Take"
                , onPress =
                    AddArtefactAnd artefact EndMove
                        |> args.onCloseOverlay
                        |> Just
                }

        else
            Layout.textButton []
                { label = "Discard " ++ Artefact.name artefact
                , onPress =
                    EndMove
                        |> args.onCloseOverlay
                        |> Just
                }
      ]
        |> Layout.column []
    , if List.length args.artefacts < Config.maxArtefacts then
        Layout.none

      else
        args.artefacts
            |> List.map
                (\oldArtefact ->
                    [ View.Artefact.info oldArtefact
                    , Layout.textButton []
                        { label = "Discard " ++ Artefact.name oldArtefact
                        , onPress =
                            RemoveArtefactAnd oldArtefact
                                (AddArtefactAnd artefact
                                    EndMove
                                )
                                |> args.onCloseOverlay
                                |> Just
                        }
                    ]
                        |> Layout.column []
                )
            |> Layout.column [ Layout.gap 16 ]
    ]
        |> Layout.column
            [ Layout.gap 16
            ]


title : { onStart : msg } -> Html msg
title args =
    [ Html.img
        [ Html.Attributes.src "assets/title.png"
        , Pixel.pixelated
        , Html.Attributes.style "width" (Config.screenMinWidth * 3 // 4 |> String.fromInt)
        ]
        []
    , Layout.text [] "Get your king to the top of the board"
        |> Layout.el
            [ Html.Attributes.style "padding" "var(--space)"
            , Html.Attributes.style "background-color" "var(--dark-gray-color)"
            ]
    , Layout.textButton []
        { label = "Start"
        , onPress = args.onStart |> Just
        }
    ]
        |> Layout.column [ Layout.gap 16 ]


gameWon : { score : Int, onRestart : msg } -> Html msg
gameWon args =
    [ [ Layout.text [] "Give me back my throne!"
            |> Layout.el
                [ Html.Attributes.style "padding" "var(--space)"
                , Html.Attributes.style "padding-left" "100px"
                , Html.Attributes.style "background-color" "var(--dark-gray-color)"
                ]
      , View.Spritesheet.bigWhiteKing
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "bottom" "-120px"
            , Html.Attributes.style "left" "-30px"
            ]
      ]
        |> Html.div [ Html.Attributes.style "position" "relative" ]
    , [ "Score"
            |> Layout.text
                ([ Html.Attributes.style "color" "var(--primary-color)" ]
                    ++ Layout.centered
                )
      , args.score
            |> String.fromInt
            |> Layout.text
                ([ Html.Attributes.style "color" "var(--primary-color)"
                 , Html.Attributes.style "font-size" "128px"
                 ]
                    ++ Layout.centered
                )
      ]
        |> Layout.column []
    , [ Layout.text [] "You wish, you just broke out of prison! But you may sleep im my bed tonight."
            |> Layout.el
                [ Html.Attributes.style "padding" "var(--space)"
                , Html.Attributes.style "padding-right" "100px"
                , Html.Attributes.style "background-color" "var(--dark-gray-color)"
                ]
      , View.Spritesheet.bigBlackQueen
            [ Html.Attributes.style "position" "absolute"
            , Html.Attributes.style "top" "-100px"
            , Html.Attributes.style "right" "-30px"
            ]
      ]
        |> Html.div [ Html.Attributes.style "position" "relative" ]
    , Layout.textButton []
        { label = "Thanks for playing"
        , onPress = Just args.onRestart
        }
    ]
        |> Layout.column [ Layout.gap 32 ]
