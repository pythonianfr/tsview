module Info exposing (main)

import Browser
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode as D
import Url.Builder as UB


metanames =
    [ "tzaware"
    , "index_type"
    , "index_dtype"
    , "value_type"
    , "value_dtype"
    , "supervision_status"
    , "index_names" -- deprecated but might still be there
    ]


type MetaVal
    = MString String
    | MInt Int
    | MFloat Float
    | MBool Bool
    | MList (List MetaVal)


metavaltostring mv =
    case mv of
        MString s -> s
        MInt i -> String.fromInt i
        MFloat f -> String.fromFloat f
        MBool b -> showbool b
        MList l -> String.join ", " <| List.map metavaltostring l


type alias StdMetadata =
    Dict String MetaVal


type alias UserMetadata =
    Dict String MetaVal


type alias Logentry =
    { rev : Int
    , author : String
    , date : String
    , meta : UserMetadata
    }


type alias Model =
    { baseurl : String
    , name : String
    , errors : List String
    , meta : StdMetadata
    , usermeta : UserMetadata
    , formula_expanded : Bool
    , formula : Maybe String
    , formula_components : List (String, String)
    , log : List Logentry
    }


type Msg
    = GotMeta (Result Http.Error String)
    | GotFormula (Result Http.Error String)
    | CodeHighlight (Result Http.Error String)
    | Components (Result Http.Error String)
    | GotLog (Result Http.Error String)
    | ToggleExpansion


pygmentyze model formula =
    Http.post
        { url =
              UB.crossOrigin
              model.baseurl
              [ "tsformula", "pygmentize" ]
              []
        , body = Http.stringBody "text/plain" formula
        , expect = Http.expectString CodeHighlight
        }


components model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "tsinfo", "finder" ]
              [ UB.string "name" model.name
              , UB.int "expanded" (if model.formula_expanded then 1 else 0)
              ]
        , expect = Http.expectString Components
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
            case D.decodeString decodemeta result of
                Ok allmeta ->
                    let
                        stdmeta = Dict.filter (\k v -> (List.member k metanames)) allmeta
                        usermeta = Dict.filter (\k v -> not (List.member k metanames)) allmeta
                        newmodel =
                            { model
                                | meta = stdmeta
                                , usermeta = usermeta
                            }
                        cmd = if supervision newmodel == "formula"
                              then getformula model
                              else getlog model.baseurl model.name
                    in ( newmodel, cmd )
                Err err ->
                    ( { model | errors = List.append model.errors [D.errorToString err] }
                    , Cmd.none
                    )

        GotMeta (Err err) ->
            ( { model | errors = List.append model.errors [unwraperror err] }
            , Cmd.none
            )

        GotFormula (Ok rawformula) ->
            let
                formula = Result.withDefault "" (D.decodeString D.string rawformula)
            in
            ( model
            , Cmd.batch [ pygmentyze model formula
                        , components model
                        ]
            )

        GotFormula (Err error) ->
            ( { model | errors = List.append model.errors [unwraperror error] }
            , Cmd.none
            )

        CodeHighlight (Ok rawformula) ->
            let
                formula = Result.withDefault "" (D.decodeString D.string rawformula)
            in
            ( { model | formula = Just formula }
            , Cmd.none
            )

        CodeHighlight (Err error) ->
            ( { model | errors = List.append model.errors [unwraperror error] }
            , Cmd.none
            )

        Components (Ok compos) ->
            let
                complist = Result.withDefault []
                           (D.decodeString (D.keyValuePairs D.string) compos)
            in
            ( { model | formula_components = complist }
            , Cmd.none
            )

        Components (Err error) ->
            ( { model | errors = List.append model.errors [unwraperror error] }
            , Cmd.none
            )

        GotLog (Ok logs) ->
            ( { model | log = Result.withDefault [] (D.decodeString decodelog logs) }
            , Cmd.none
            )

        GotLog (Err error) ->
            ( { model | errors = List.append model.errors [unwraperror error] }
            , Cmd.none
            )

        ToggleExpansion ->
            if model.formula_expanded then
                ( { model | formula_expanded = False }
                , getformula { model | formula_expanded = False }
                )
            else
                ( { model | formula_expanded = True }
                , getformula { model | formula_expanded = True }
                )


showbool b =
    if b then "true" else "false"


supervision model =
    case Dict.get "supervision_status" model.meta of
        Nothing -> "formula"
        Just x -> metavaltostring x


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
                , div [ A.class "custom-control custom-switch"
                      , A.title (if model.formula_expanded
                                 then "unexpand the formula" else "expand the formula")
                      ]
                     [ input
                           [ A.attribute "type" "checkbox"
                           , A.class "custom-control-input"
                           , A.id "expand-formula"
                           , onClick ToggleExpansion
                           ] []
                     , label
                         [ A.class "custom-control-label"
                         , A.for "expand-formula"
                         ]
                         [ text (if model.formula_expanded
                                 then "expanded" else "unexpanded")
                         ]
                     ]
                , span [] (tovirtualdom formula)
                ]


metadicttostring d =
    let
        builditem ab =
            let
                first = Tuple.first ab
                second = metavaltostring (Tuple.second ab)
            in
                first ++ " → " ++ second
    in
    String.join "," (List.map builditem (Dict.toList d))


viewlogentry entry =
    tr []
        [ th [A.scope "row"] [text (String.fromInt entry.rev)]
        , td [] [text entry.author]
        , td [] [text entry.date]
        , td [] [text (metadicttostring entry.meta)]
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


dget name dict =
    case Dict.get name dict of
        Nothing -> ""
        Just something -> metavaltostring something


viewmeta model =
    let
        hidden = ["index_names", "index_dtype", "value_dtype"]
        elt name =
            li [] [text (name ++ " → " ++ (dget name model.meta))]
    in
    div []
    [ h2 [] [text "Metadata"]
    , ul [] <| List.map elt <| List.filter (\x -> not <| List.member x hidden) metanames
    ]


viewusermeta model =
    let
        elt (k, v) =
            li [] [text <| (k ++ " → " ++ (metavaltostring v))]
    in
    if Dict.isEmpty model.usermeta then div [] [] else
    div []
        [ h2 [] [text "User Metadata"]
        , ul [] (List.map elt (Dict.toList model.usermeta))
        ]


viewcomponents model =
    let
        elt (name, expr) =
            li []
                [ a [ A.href (UB.crossOrigin model.baseurl
                                  ["tsinfo"]
                                  [UB.string "name" name]
                           )
                    ] [text name]
                , span [] [text (" → " ++ expr)]
                ]
    in
    if supervision model == "formula" then
        div []
            [ h2 [] [(text "Components")]
            , ul [] (List.map elt model.formula_components)
            ]
    else div [] [ ]


viewerrors model =
    if List.length model.errors > 0 then
    div []
        [ h2 [] [text "Errors"]
        , p [] (List.map text model.errors)
        ]
    else span [] []


view : Model -> Html Msg
view model =
    div []
        [h1 [] [text ("Series " ++ model.name)]
        , viewmeta model
        , viewusermeta model
        , viewformula model
        , viewlog model
        , viewcomponents model
        , viewerrors model
        ]


decodemetaval : D.Decoder MetaVal
decodemetaval =
    D.oneOf
        [ D.map MString D.string
        , D.map MInt D.int
        , D.map MFloat D.float
        , D.map MBool D.bool
        , D.map MList (D.succeed [])
        ]


decodemeta : D.Decoder UserMetadata
decodemeta =
    D.dict decodemetaval


getmetadata : String -> String-> Cmd Msg
getmetadata urlprefix name  =
    Http.get
        { expect =
              Http.expectString GotMeta
        , url =
            UB.crossOrigin urlprefix
                [ "api", "series", "metadata" ]
                [ UB.string "name" name
                , UB.int "all" 1 ]
        }


getformula : Model -> Cmd Msg
getformula model  =
    Http.get
        { expect = Http.expectString  GotFormula
        , url =
            UB.crossOrigin model.baseurl
                [ "api", "series", "formula" ]
                [ UB.string "name" model.name
                , UB.int "expanded" (if model.formula_expanded then 1 else 0)
                ]
        }


decodelogentry : D.Decoder Logentry
decodelogentry =
    D.map4 Logentry
        (D.field "rev" D.int)
        (D.field "author" D.string)
        (D.field "date" D.string)
        (D.field "meta" (D.dict decodemetaval))


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
                     []
                     Dict.empty
                     Dict.empty
                     False
                     Nothing
                     []
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
