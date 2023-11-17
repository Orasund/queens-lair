module Action exposing (..)

import Artefact exposing (Artefact(..))
import Piece exposing (Piece(..))
import Settings exposing (Settings)


type Action
    = ResetLevel
    | NextLevel (List Piece)
    | RemoveArtefactAnd Artefact Action
    | AddArtefactAnd Artefact Action
    | OverrideMovement Movement
    | PlaceChest Action
    | PlacePiece Piece
    | UndoMove
    | FindArtefact ( Int, Int )
    | EndMove
    | DoNothing
    | RestartGame Settings


type Movement
    = PieceMovement Piece
    | ToChest


fromArtefact : Artefact -> Action
fromArtefact artefact =
    case artefact of
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

        -- DowsingRod ->
        --    ToChest |> OverrideMovement
        Bible ->
            Bishop
                |> PieceMovement
                |> OverrideMovement

        PocketMoney ->
            PlaceChest (PlaceChest DoNothing)



--TinSoldier ->
--     PlacePiece Pawn
