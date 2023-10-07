module View exposing (..)

import Html exposing (Html)


stylesheet : Html msg
stylesheet =
    --In-Elm Stylesheet is usually easier to load by itch.io
    "" |> Html.text |> List.singleton |> Html.node "style" []
