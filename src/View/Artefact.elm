module View.Artefact exposing (..)

import Artefact exposing (Artefact)
import Html exposing (Html)
import Html.Attributes
import Layout


toHtml : { onActivate : Artefact -> msg } -> List Artefact -> Html msg
toHtml args list =
    list
        |> List.map
            (\artefact ->
                [ info artefact
                , Layout.textButton []
                    { label = "Activate"
                    , onPress = args.onActivate artefact |> Just
                    }
                ]
                    |> Layout.column []
            )
        |> Layout.column [ Layout.gap 16 ]


info : Artefact -> Html msg
info artefact =
    [ Artefact.name artefact
        |> Layout.text [ Html.Attributes.style "font-size" "1.2rem" ]
    , Artefact.description artefact
        |> Layout.text []
    ]
        |> Layout.column
            [ Html.Attributes.style "padding" "var(--small-space)"
            , Html.Attributes.style "background-color" "var(--gray-color)"
            ]
