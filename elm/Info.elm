module Info exposing (main)

import Browser
import Common
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (..)
import Html.Attributes as A
import Html.Parser
import Html.Parser.Util
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


type alias Logentry =
    { rev : Int
    , author : String
    , date : String
    , meta : Dict String String
    }


type alias Model =
    { baseurl : String
    , name : String
    , meta_error : String
    , meta : Metadata
    , formula_error : String
    , formula : Maybe String
    , log_error : String
    , log : List Logentry
    }


type Msg
    = GotMeta (Result String Metadata)
    | GotFormula (Result String String)
    | CodeHighlight (Result String String)
    | GotLog (Result Http.Error String)


pygmentyze model formula =
    Http.post
        { url =
              UB.crossOrigin
              model.baseurl
              [ "tsformula", "pygmentize" ]
              []
        , body = Http.stringBody "text/plain" formula
        , expect = Common.expectJsonMessage CodeHighlight D.string
        }


unwraperror : Http.Error -> String
unwraperror resp =
    case resp of
        Http.BadUrl x -> "bad url: " ++ x
        Http.Timeout -> "the query timed out"
        Http.NetworkError -> "there was a network error"
        Http.BadStatus val -> "we got a bad status answer: " ++ String.fromInt val
        Http.BadBody body -> "we got a bad body: " ++ body


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMeta (Ok result) ->
            let
                newmodel = { model | meta = result }
                cmd = if
                    supervision newmodel == "formula" then
                   getformula model.baseurl model.name else
                   getlog model.baseurl model.name
            in
                ( newmodel
                , cmd
                )

        GotMeta (Err result) ->
            ( { model | meta_error = result }
            , Cmd.none
            )

        GotFormula (Ok formula) ->
            (model
            , pygmentyze model formula
            )

        GotFormula (Err error) ->
            ( { model | formula_error = error}
            , Cmd.none
            )

        CodeHighlight (Ok formula) ->
            ( { model | formula = Just formula }
            , Cmd.none
            )

        CodeHighlight (Err error) ->
            ( { model | formula_error = error}
            , Cmd.none
            )

        GotLog (Ok logs) ->
            ( { model | log = Result.withDefault [] (D.decodeString decodelog logs) }
            , Cmd.none
            )

        GotLog (Err error) ->
            ( { model | log_error = unwraperror error }
            , Cmd.none
            )


showbool b =
    if b then "true" else "false"


supervision model =
    case model.meta.supervision_status of
        Nothing -> "formula"
        Just x -> x


tovirtualdom : String -> List (Html.Html msg)
tovirtualdom html =
    case Html.Parser.run html of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes
        Err x ->
            [div [] [text "could not parse the formula"]]


viewformula model =
    case model.formula of
        Nothing -> div [] []
        Just formula ->
            div [] [
                 h2 [] [text "Formula"]
                , span [] (tovirtualdom formula)
                ]


dicttostring d =
    let
        builditem ab = Tuple.first ab ++ " → " ++ Tuple.second ab
    in
    String.join "," (List.map builditem (Dict.toList d))


viewlogentry entry =
    tr []
        [ th [A.scope "row"] [text (String.fromInt entry.rev)]
        , td [] [text entry.author]
        , td [] [text entry.date]
        , td [] [text (dicttostring entry.meta)]
        ]


viewlog model =
    if List.length model.log > 0 then
        div [] [
             h2 [] [text "History Log"]
            , table [A.class "table table-striped table-hover table-sm"]
                 [ thead []
                       [td [A.scope "col"] [text "#"]
                       , td [A.scope "col"] [text "author"]
                       , td [A.scope "col"] [text "date"]
                       , td [A.scope "col"] [text "meta"]
                       ]
                 , tbody []
                     (List.map viewlogentry (List.reverse model.log))
                 ]
            ]
    else div [] []


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
        , viewlog model
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
        { expect = Common.expectJsonMessage GotFormula D.string
        , url =
            UB.crossOrigin urlprefix
                [ "api", "series", "formula" ]
                [ UB.string "name" name]
        }


decodelogentry : D.Decoder Logentry
decodelogentry =
    D.map4 Logentry
        (D.field "rev" D.int)
        (D.field "author" D.string)
        (D.field "date" D.string)
        (D.field "meta" (D.dict D.string))


decodelog : D.Decoder (List Logentry)
decodelog =
    D.list decodelogentry


getlog : String -> String-> Cmd Msg
getlog urlprefix name  =
    Http.get
        { expect = Http.expectString GotLog
        , url = UB.crossOrigin urlprefix
              [ "api", "series", "log" ]
              [ UB.string "name" name
              , UB.int "limit" 10 ]
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
                     ""
                     []
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
