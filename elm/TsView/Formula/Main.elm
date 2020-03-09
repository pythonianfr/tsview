module TsView.Formula.Main exposing (main)

import Browser
import Html as H exposing (Html)
import Json.Decode as D
import TsView.Formula.EditionTree.Main as EditionTree


type Msg
    = EditionTreeMsg EditionTree.Msg


type alias Model =
    { urlPrefix : String
    , editionTree : EditionTree.Model
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditionTreeMsg x ->
            Tuple.mapBoth
                (\m -> { model | editionTree = m })
                (Cmd.map EditionTreeMsg)
                (EditionTree.update x model.editionTree)


view : Model -> Html Msg
view model =
    H.map EditionTreeMsg (EditionTree.view model.editionTree)


main : Program ( String, D.Value, Maybe String ) Model Msg
main =
    let
        init ( urlPrefix, jsonSpec, formulaName ) =
            ( Model
                urlPrefix
                (EditionTree.init jsonSpec)
            , Cmd.none
            )

        sub model =
            Sub.none
    in
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = sub
        }
