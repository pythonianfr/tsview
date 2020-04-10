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
    , supervision_status : Maybe String
    }


type alias Model =
    { baseurl : String
    , name : String
    , meta_error : String
    , meta : Metadata
    , formula_error : String
    , formula : Maybe String
    }


type Msg
    = GotMeta (Result String Metadata)
    | GotFormula (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMeta (Ok result) ->
            let
                newmodel = { model | meta = result }
                cmd = if
                    supervision newmodel == "formula" then
                   getformula model.baseurl model.name else
                   Cmd.none
            in
                ( newmodel
                , cmd
                )

        GotMeta (Err result) ->
            ( { model | meta_error = result }
            , Cmd.none
            )

        GotFormula (Ok formula) ->
            ( { model | formula = Just formula }
            , Cmd.none
            )

        GotFormula (Err _) ->
            ( { model | formula_error = "request failed"}
            , Cmd.none
            )


showbool b =
    if b then "true" else "false"


supervision model =
    case model.meta.supervision_status of
        Nothing -> "formula"
        Just x -> x


viewformula model =
    case model.formula of
        Nothing -> div [] []
        Just formula ->
            div [] [
                 h2 [] [text "Formula"]
                 , span [] [text formula]
                ]


view : Model -> Html Msg
view model =
    div []
        [h1 [] [text ("Series " ++ model.name)]
        , h2 [] [text "Metadata"]
        , div [] [text model.meta_error]
        , ul [] [
              li [] [text ("tz aware    → " ++ showbool model.meta.tzaware)]
             , li [] [text ("supervision → " ++ supervision model)]
             , li [] [text ("index type  → " ++ model.meta.index_type)]
             , li [] [text ("value type  → " ++ model.meta.value_type)]
             ]
        , viewformula model
        ]


decodemeta : D.Decoder Metadata
decodemeta =
    D.map6 Metadata
        (D.field "tzaware" D.bool)
        (D.field "index_type" D.string)
        (D.field "index_dtype" D.string)
        (D.field "value_type" D.string)
        (D.field "index_dtype" D.string)
        (D.maybe (D.field "supervision_status" D.string))


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


getformula : String -> String-> Cmd Msg
getformula urlprefix name  =
    Http.get
        { expect =
              Http.expectString GotFormula
        , url =
            UB.crossOrigin urlprefix
                [ "api", "series", "formula" ]
                [ UB.string "name" name]
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
                     (Metadata False "" "" "" "" (Just ""))
                     ""
                     Nothing
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
