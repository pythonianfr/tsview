module TsView.Formula.EditionTree.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (Html)
import Json.Decode exposing (Value)
import TsView.Formula.EditionTree.Type as ET
import TsView.Formula.EditionTree.Update as Update
import TsView.Formula.EditionTree.View as View
import TsView.Formula.Spec.Type as S


type alias Msg =
    ET.Msg


type alias Model =
    ET.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Update.update


view : Model -> Html Msg
view model =
    View.viewEditionNode model.spec model.tree


init : S.Spec -> Model
init spec =
    ET.Model spec (ET.buildInitialTree spec)
