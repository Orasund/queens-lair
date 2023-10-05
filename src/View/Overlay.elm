module View.Overlay exposing (..)

import Action exposing (Action(..))
import Artefact exposing (Artefact)
import Config
import Html exposing (Html)
import Html.Attributes
import Layout
import Pixel
import View.Artefact


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
