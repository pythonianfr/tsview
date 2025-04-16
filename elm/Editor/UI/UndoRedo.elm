module Editor.UI.UndoRedo exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE

import UndoList as UL

import Util


type Msg
    = Undo
    | Redo


type alias UndoList a = UL.UndoList a


makeButton_ : Msg -> UndoList a -> Html Msg
makeButton_ msg undoList =
    let
        (code, title, enabled) = case msg of
            Undo -> (10226, "Undo", UL.hasPast undoList)
                -- ANTICLOCKWISE GAPPED CIRCLE ARROW

            Redo -> (10227, "Redo", UL.hasFuture undoList)
                -- CLOCKWISE GAPPED CIRCLE ARROW

    in H.button
        [ HA.title title
        , HA.disabled (not enabled)
        , HE.onClick msg
        ]
        [ H.text <| Util.fromCharCode code ]


renderButtons : UL.UndoList a -> Html Msg
renderButtons undoList =
    H.div
        [ HA.class "undo_redo" ]
        [ makeButton_ Undo undoList
        , makeButton_ Redo undoList
        ]
