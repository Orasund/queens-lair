port module Main exposing (main)

import Action exposing (Action(..), Movement)
import Artefact exposing (Artefact(..))
import Browser
import Config
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Layout
import Level exposing (Level)
import Level.Generate
import Overlay exposing (Overlay(..))
import Piece exposing (Piece(..))
import Process
import Random exposing (Seed)
import Set
import Settings exposing (Settings)
import Task
import View.Artefact
import View.Level
import View.Overlay
import View.Shop


port playTheme : () -> Cmd msg


type alias Model =
    { level : Level
    , artefacts : Dict String Artefact
    , selected : Maybe ( Int, Int )
    , levelCount : Int
    , overlay : Maybe Overlay
    , seed : Seed
    , movementOverride : Maybe Movement
    , score : Int
    , lives : Int
    , settings : Settings
    }


type Msg
    = Select (Maybe ( Int, Int ))
    | RequestOpponentMove
    | GotSeed Seed
    | EndLevel
    | Activate Artefact
    | CloseOverlay Action
    | LooseLive
    | Restart


initModel : Settings -> Model
initModel settings =
    let
        lv =
            1

        party =
            [ King ]

        ( level, seed ) =
            Random.step
                (Level.Generate.generateByLevel lv
                    settings
                    party
                )
                (Random.initialSeed 42)
    in
    { level = level
    , artefacts = Dict.empty
    , selected = Nothing
    , levelCount = lv
    , overlay = Just NewGame
    , seed = seed
    , movementOverride = Nothing
    , score = 0
    , lives =
        if settings.withLives then
            3

        else
            1
    , settings = settings
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( initModel Settings.modern
    , Random.generate GotSeed Random.independentSeed
    )


startLevel : List Piece -> Model -> ( Model, Cmd Msg )
startLevel party model =
    let
        levelCount =
            model.levelCount + 1

        ( level, seed ) =
            Random.step
                (Level.Generate.generateByLevel levelCount
                    model.settings
                    (if List.member King party then
                        party

                     else
                        King :: party
                    )
                )
                model.seed
    in
    ( { model
        | level = level
        , seed = seed
        , levelCount = levelCount
        , selected = Nothing
        , overlay = Nothing
        , movementOverride = Nothing
      }
    , Cmd.none
    )


applyAction : Action -> Model -> ( Model, Cmd Msg )
applyAction action model =
    case action of
        ResetLevel ->
            startLevel
                (model.level.board
                    |> Dict.filter (\_ square -> square.isWhite)
                    |> Dict.toList
                    |> List.map (\( _, { piece } ) -> piece)
                )
                { model
                    | levelCount = model.levelCount - 1
                }

        NextLevel party ->
            startLevel party model

        RemoveArtefactAnd artefact action2 ->
            { model | artefacts = model.artefacts |> Dict.remove (Artefact.name artefact) }
                |> applyAction action2

        AddArtefactAnd artefact action2 ->
            { model | artefacts = model.artefacts |> Dict.insert (Artefact.name artefact) artefact }
                |> applyAction action2

        UndoMove ->
            ( { model | level = model.level |> Level.undo }
            , Cmd.none
            )

        FindArtefact pos ->
            ( Random.step
                (case
                    Artefact.list
                        |> List.filter
                            (\artefact ->
                                model.artefacts
                                    |> Dict.values
                                    |> List.member artefact
                                    |> not
                            )
                 of
                    head :: tail ->
                        Random.uniform head tail

                    [] ->
                        Random.constant FingerPistol
                )
                model.seed
                |> (\( artefact, seed ) ->
                        { model
                            | overlay = Just (FoundArtefactOverlay artefact)
                            , level = model.level |> (\l -> { l | loot = Set.remove pos l.loot })
                            , seed = seed
                        }
                   )
            , Cmd.none
            )

        EndMove ->
            ( { model | movementOverride = Nothing }
            , Process.sleep 100
                |> Task.perform (\() -> RequestOpponentMove)
            )

        OverrideMovement movement ->
            ( { model | movementOverride = Just movement }
            , Cmd.none
            )

        PlaceChest action2 ->
            (case
                Set.diff
                    (List.range 0 (Config.boardSize - 1)
                        |> List.concatMap
                            (\x ->
                                List.range 0 (Config.boardSize - 1)
                                    |> List.map (Tuple.pair x)
                            )
                        |> Set.fromList
                    )
                    model.level.loot
                    |> Set.toList
             of
                head :: tail ->
                    Random.step (Random.uniform head tail) model.seed
                        |> (\( pos, seed ) ->
                                { model
                                    | seed = seed
                                    , level =
                                        model.level
                                            |> (\l -> { l | loot = Set.insert pos l.loot })
                                }
                           )

                [] ->
                    model
            )
                |> applyAction action2

        PlacePiece piece ->
            case
                List.range 0 (Config.boardSize - 1)
                    |> List.concatMap
                        (\x ->
                            List.range 0 (Config.boardSize - 1)
                                |> List.map (Tuple.pair x)
                        )
                    |> List.filter (\pos -> model.level.board |> Dict.get pos |> (==) Nothing)
            of
                head :: tail ->
                    Random.step (Random.uniform head tail) model.seed
                        |> (\( pos, seed ) ->
                                { model
                                    | seed = seed
                                    , level =
                                        model.level
                                            |> (\l ->
                                                    { l
                                                        | board =
                                                            l.board
                                                                |> Dict.insert pos { isWhite = True, piece = piece }
                                                    }
                                               )
                                }
                           )
                        |> (\m -> ( m, Cmd.none ))

                [] ->
                    ( model, Cmd.none )

        RestartGame settings ->
            ( initModel settings |> (\m -> { m | overlay = Nothing })
            , playTheme ()
            )

        DoNothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Select maybe ->
            case model.selected of
                Nothing ->
                    ( { model | selected = maybe }, Cmd.none )

                Just selected ->
                    case maybe of
                        Nothing ->
                            ( { model | selected = maybe }, Cmd.none )

                        Just to ->
                            if
                                model.level.board
                                    |> Dict.get to
                                    |> Maybe.map .isWhite
                                    |> (==) (Just True)
                            then
                                ( { model | selected = maybe }, Cmd.none )

                            else
                                model.level
                                    |> Level.move { from = selected, to = to }
                                    |> (\level ->
                                            { model
                                                | level = level
                                                , selected = Nothing
                                            }
                                       )
                                    |> applyAction
                                        (if Set.member to model.level.loot then
                                            FindArtefact to

                                         else
                                            EndMove
                                        )

        RequestOpponentMove ->
            (case model.level |> Level.findNextMove of
                Just args ->
                    { model
                        | level =
                            Level.move args model.level
                    }

                Nothing ->
                    case model.level |> Level.possibleMoves { isYourTurn = True } of
                        head :: tail ->
                            Random.step (Random.uniform head tail)
                                model.seed
                                |> (\( move, seed ) ->
                                        { model
                                            | level =
                                                model.level
                                                    |> Level.move move
                                            , seed = seed
                                        }
                                   )

                        [] ->
                            model
            )
                |> (\m -> ( m, Cmd.none ))

        GotSeed seed ->
            ( { model | seed = seed }, Cmd.none )

        EndLevel ->
            let
                score =
                    model.level.board
                        |> Dict.filter (\_ square -> not square.isWhite)
                        |> Dict.toList
                        |> List.map (\( _, square ) -> Piece.value square.piece)
                        |> List.sum
                        |> (+) (model.level.loot |> Set.size |> (*) Config.chessValue)
                        |> (+) model.score
            in
            if model.levelCount >= 8 then
                ( { model
                    | overlay = Just GameWon
                    , score = score
                  }
                , Cmd.none
                )

            else
                ( model.level.board
                    |> Dict.filter (\_ square -> square.isWhite)
                    |> Dict.toList
                    |> List.map (\( _, { piece } ) -> piece)
                    |> (\party ->
                            { model
                                | overlay = ShopOverlay { party = party } |> Just
                                , score = score
                            }
                       )
                , Cmd.none
                )

        Activate artefact ->
            applyAction (Action.fromArtefact artefact)
                { model
                    | artefacts = model.artefacts |> Dict.remove (Artefact.name artefact)
                }

        CloseOverlay action ->
            applyAction action
                { model | overlay = Nothing }

        LooseLive ->
            if model.settings.withLives && model.lives > 2 then
                applyAction ResetLevel
                    { model | lives = model.lives - 1 }

            else
                ( initModel model.settings
                , Cmd.none
                )

        Restart ->
            ( initModel model.settings
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    (case model.overlay of
        Just (ShopOverlay { party }) ->
            View.Shop.toHtml
                { party = party
                , onCloseOverlay = CloseOverlay
                }

        Just (FoundArtefactOverlay artefact) ->
            View.Overlay.foundArtefact
                { onCloseOverlay = CloseOverlay
                , artefacts = model.artefacts |> Dict.values
                }
                artefact

        Just NewGame ->
            View.Overlay.title
                { onStart =
                    \settings ->
                        CloseOverlay (RestartGame settings)
                }

        Just GameWon ->
            View.Overlay.gameWon
                { score = model.score
                , onRestart = Restart
                }

        Nothing ->
            [ [ "Level "
                    ++ String.fromInt model.levelCount
                    ++ ": "
                    ++ View.Level.name model.levelCount
                    |> Layout.text [ Layout.contentCentered ]
              , [ "Lives "
                    ++ String.fromInt model.lives
                    |> Layout.text []
                , "Score "
                    ++ String.fromInt model.score
                    |> Layout.text []
                ]
                    |> Layout.row
                        [ Layout.gap 4
                        , Layout.contentWithSpaceBetween
                        ]

              {--Html.img
                    [ Html.Attributes.src "assets/small_heart.png"
                    , Pixel.pixelated
                    , Html.Attributes.style "width" (30 |> String.fromInt)
                    , Html.Attributes.style "height" (30 |> String.fromInt)

                    -- , Html.Attributes.style "position" "absolute"
                    --, Html.Attributes.style "left" (String.fromInt x ++ "px")
                    --, Html.Attributes.style "bottom" (String.fromInt y ++ "px")
                    ]
                    []
                    |> List.repeat model.lives
                    |> Layout.row
                        [ Layout.gap 5
                        , Html.Attributes.style "align-items" "end"
                        ]--}
              ]
                |> Layout.column
                    [ Html.Attributes.style "background-color" "var(--gray-color)"
                    , Html.Attributes.style "padding" "var(--small-space)"
                    , Html.Attributes.style "gap" "var(--space)"
                    ]
            , View.Level.toHtml
                { selected = model.selected
                , onSelect = Select
                , movementOverride = model.movementOverride
                }
                model.level
            , if Level.isKingBehindLine model.level then
                [ model.level.board
                    |> Dict.filter (\_ square -> not square.isWhite)
                    |> Dict.toList
                    |> List.map
                        (\( _, square ) ->
                            [ Piece.name square.piece
                                |> Layout.text []
                            , Piece.value square.piece
                                |> (\n ->
                                        if n == 1 then
                                            "1 Point"

                                        else
                                            String.fromInt n ++ " Points"
                                   )
                                |> Layout.text []
                            ]
                                |> Layout.row [ Layout.contentWithSpaceBetween ]
                        )
                    |> (Set.size model.level.loot
                            |> (\n ->
                                    if n == 0 then
                                        identity

                                    else
                                        [ (if n == 1 then
                                            "1 Chest"

                                           else
                                            String.fromInt n ++ " Chests"
                                          )
                                            |> Layout.text []
                                        , Config.chessValue
                                            * n
                                            |> (\p ->
                                                    if p == 1 then
                                                        "1 Point"

                                                    else
                                                        String.fromInt p ++ " Points"
                                               )
                                            |> Layout.text []
                                        ]
                                            |> Layout.row [ Layout.contentWithSpaceBetween ]
                                            |> (::)
                               )
                       )
                    |> Layout.column
                        [ Layout.gap 8
                        , Html.Attributes.style "padding" "var(--small-space)"
                        , Html.Attributes.style "background-color" "var(--gray-color)"
                        ]
                , Layout.textButton []
                    { label = "Next Level"
                    , onPress = Just EndLevel
                    }
                ]
                    |> Layout.column [ Layout.gap 16 ]

              else if Level.isLost model.level then
                Layout.textButton
                    [ Html.Attributes.style "background-color" "var(--red-color)"
                    ]
                    { label = "You died"
                    , onPress = Just LooseLive
                    }

              else
                model.artefacts
                    |> Dict.values
                    |> View.Artefact.toHtml
                        { onActivate = Activate }
            ]
                |> Layout.column
                    [ Layout.gap 16
                    ]
    )
        |> Layout.el
            [ Html.Attributes.style "width" (String.fromFloat Config.screenMinWidth)
            , Layout.contentCentered
            ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
