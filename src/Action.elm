module Action exposing (..)

import Artefact exposing (Artefact(..))
import Piece exposing (Piece(..))


type Action
    = ResetLevel
    | NextLevel (List Piece)
    | RemoveArtefactAnd Artefact Action
    | AddArtefactAnd Artefact Action
    | OverrideMovement Movement
    | PlaceChest
    | UndoMove
    | FindArtefact ( Int, Int )
    | EndMove
    | DoNothing


type Movement
    = PieceMovement Piece
    | ToChest


fromArtefact : Artefact -> Action
fromArtefact artefact =
    case artefact of
        EscapeRope ->
            ResetLevel

        PoliceBox ->
            UndoMove

        Coconuts ->
            Knight
                |> PieceMovement
                |> OverrideMovement

        FingerPistol ->
            Pawn
                |> PieceMovement
                |> OverrideMovement

        IronThrone ->
            King
                |> PieceMovement
                |> OverrideMovement

        DowsingRod ->
            ToChest |> OverrideMovement

        Bible ->
            Bishop
                |> PieceMovement
                |> OverrideMovement

        PocketMoney ->
            PlaceChest
