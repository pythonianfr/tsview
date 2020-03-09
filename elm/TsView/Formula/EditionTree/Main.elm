module TsView.Formula.EditionTree.Main exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Either exposing (Either(..))
import Html exposing (Html)
import Json.Decode as D
import TsView.Formula.EditionTree.Type as T
import TsView.Formula.EditionTree.Update as Update
import TsView.Formula.EditionTree.View as View
import TsView.Formula.Spec.Parser exposing (parseSpecValue)
import TsView.Formula.Spec.Type as S


type alias Msg =
    T.Msg


type alias Model =
    T.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Update.update


view : Model -> Html Msg
view =
    View.view


init : D.Value -> Model
init =
    let
        initModel ( spec, errors ) =
            T.Model spec errors
    in
    parseSpecValue
        >> Either.unpack
            (\( spec, errors ) -> ( spec, Just errors ))
            (\spec -> ( spec, Nothing ))
        >> initModel
