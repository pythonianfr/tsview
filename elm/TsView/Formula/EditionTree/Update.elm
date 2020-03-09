module TsView.Formula.EditionTree.Update exposing (update)

import TsView.Formula.EditionTree.Type exposing (Model, Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
