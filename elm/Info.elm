module Info exposing (main)

import Browser
import Common
import Dict exposing (Dict)
import Html exposing (..)
import Http
import Json.Decode as D
import Url.Builder as UB


type alias Metadata =
    { tzaware : Bool
    , index_type : String
    , index_dtype : String
    , value_type : String
    , value_dtype : String
    , supervision_status : String
    }


type alias Model =
    { baseurl : String
    , name : String
    , error : String
    , metadata : Metadata
    }


type Msg
    = GotMeta (Result String Metadata)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMeta (Ok result) ->
            ( { model | metadata = result }
            , Cmd.none
            )

        GotMeta (Err result) ->
            ( { model | error = result }
            , Cmd.none
            )


showbool b =
    if b then "true" else "false"


view : Model -> Html Msg
view model =
    div []
        [h1 [] [text ("Series " ++ model.name)]
        , h2 [] [text "Metadata"]
        , div [] [text model.error]
        , ul [] [
              li [] [text ("tz aware    → " ++ showbool model.metadata.tzaware)]
             , li [] [text ("supervision → " ++ model.metadata.supervision_status)]
             , li [] [text ("index type  → " ++ model.metadata.index_type)]
             , li [] [text ("value type  → " ++ model.metadata.value_type)]
             ]
        ]


decodemeta : D.Decoder Metadata
decodemeta =
    D.map6 Metadata
        (D.at ["tzaware"] D.bool)
        (D.at ["index_type"] D.string)
        (D.at ["index_dtype"] D.string)
        (D.at ["value_type"] D.string)
        (D.at ["index_dtype"] D.string)
        (D.at ["supervision_status"] D.string)


getmetadata : String -> String-> Cmd Msg
getmetadata urlprefix name  =
    Http.get
        { expect =
              (Common.expectJsonMessage GotMeta)
              decodemeta
        , url =
            UB.crossOrigin urlprefix
                [ "api", "series", "metadata" ]
                [ UB.string "name" name
                , UB.int "all" 1 ]
        }


type alias Input =
    { baseurl : String
    , name : String
    }


main : Program Input  Model Msg
main =
       let
           init input =
               ( Model
                     input.baseurl
                     input.name
                     ""
                     (Metadata False "" "" "" "" "")
               ,
                   getmetadata input.baseurl input.name
               )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
