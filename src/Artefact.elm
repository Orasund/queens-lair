module Artefact exposing (..)


type Artefact
    = EscapeRope
    | PoliceBox
    | Coconuts
    | FingerPistol
    | IronThrone
    | DowsingRod
    | Bible
    | PocketMoney


list : List Artefact
list =
    [ EscapeRope
    , PoliceBox
    , Coconuts
    , FingerPistol
    , IronThrone
    , DowsingRod
    , Bible
    , PocketMoney
    ]


name : Artefact -> String
name item =
    case item of
        EscapeRope ->
            "Escape Rope"

        PoliceBox ->
            "Police Box"

        Coconuts ->
            "Coconuts"

        FingerPistol ->
            "Finger Pistol"

        IronThrone ->
            "Iron Throne"

        DowsingRod ->
            "Dowsing Rod"

        Bible ->
            "Bible"

        PocketMoney ->
            "Pocket Money"


description : Artefact -> String
description item =
    case item of
        EscapeRope ->
            "Restart the level"

        PoliceBox ->
            "Undo your last move"

        Coconuts ->
            "Move one piece like a knight"

        FingerPistol ->
            "Move one piece like a pawn"

        IronThrone ->
            "Move one piece like a king"

        DowsingRod ->
            "Move one piece to the chest"

        Bible ->
            "Move one piece like a bishop"

        PocketMoney ->
            "Spawn another chest"
