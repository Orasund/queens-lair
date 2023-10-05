module Artefact exposing (..)


type Artefact
    = Coconuts
    | FingerPistol
    | IronThrone
    | DowsingRod
    | Bible
    | PocketMoney
    | TinSoldier


list : List Artefact
list =
    [ Coconuts
    , FingerPistol
    , IronThrone
    , DowsingRod
    , Bible
    , PocketMoney
    , TinSoldier
    ]


name : Artefact -> String
name item =
    case item of
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

        TinSoldier ->
            "Tin Soldier"


description : Artefact -> String
description item =
    case item of
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
            "Spawn two chests"

        TinSoldier ->
            "Add a white pawn to the board"
