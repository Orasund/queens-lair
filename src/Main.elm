module Main exposing (..)

import Browser
import Config
import Game exposing (Game)
import Html exposing (Html)
import Html.Attributes
import Layout
import Overlay exposing (Overlay(..))
import Random exposing (Generator, Seed)
import View
import View.Overlay


type alias Model =
    { game : Game
    , overlay : Maybe Overlay
    , seed : Seed
    }


type Msg
    = NewGame
    | SetOverlay (Maybe Overlay)
    | GotSeed Seed


apply : Model -> Generator Model -> Model
apply { seed } generator =
    let
        ( model, newSeed ) =
            Random.step generator seed
    in
    { model | seed = newSeed }


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.new
      , seed = Random.initialSeed 42
      , overlay = Just GameMenu
      }
    , Cmd.none
    )


newGame : Model -> Model
newGame model =
    { model | game = Game.new }
        |> setOverlay Nothing


gotSeed : Seed -> Model -> Model
gotSeed seed model =
    { model | seed = seed }


setOverlay : Maybe Overlay -> Model -> Model
setOverlay maybeOverlay model =
    { model | overlay = maybeOverlay }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        withNoCmd a =
            ( a, Cmd.none )
    in
    case msg of
        NewGame ->
            newGame model |> withNoCmd

        GotSeed seed ->
            model |> gotSeed seed |> withNoCmd

        SetOverlay maybeOverlay ->
            model |> setOverlay maybeOverlay |> withNoCmd


viewOverlay : Model -> Overlay -> Html Msg
viewOverlay _ overlay =
    case overlay of
        GameMenu ->
            View.Overlay.gameMenu
                { startGame = NewGame }


view :
    Model
    ->
        { title : String
        , body : List (Html Msg)
        }
view model =
    let
        content =
            Html.text ""
    in
    { title = Config.title
    , body =
        [ View.viewportMeta

        --, View.stylesheet
        , model.overlay
            |> Maybe.map (viewOverlay model)
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
            |> (::) (content |> Layout.el [ Html.Attributes.class "content" ])
            |> Html.div
                [ Html.Attributes.style "width" (String.fromFloat Config.screenMinWidth ++ "px")
                , Html.Attributes.style "height" (String.fromFloat Config.screenMinHeight ++ "px")
                , Html.Attributes.class "container"
                ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
