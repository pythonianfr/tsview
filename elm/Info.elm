module Info exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Parser
import Html.Parser.Util
import Http
import Json.Decode as D
import Json.Encode as E
import Metadata as M
import Plotter exposing
    ( getplotdata
    , seriesdecoder
    , scatterplot
    , plotargs
    , Series
    )
import Url.Builder as UB


type alias Logentry =
    { rev : Int
    , author : String
    , date : String
    , meta : M.UserMetadata
    }


type alias Model =
    { baseurl : String
    , name : String
    -- metadata edition
    , canwrite : Bool
    , editing : Bool
    -- all errors
    , errors : List String
    -- metadata, ventilated by std (system) and user
    , meta : M.StdMetadata
    , usermeta : M.UserMetadata
    -- formula
    , formula_expanded : Bool
    , formula : Maybe String
    , formula_components : List (String, String)
    -- log
    , log : List Logentry
    -- plot
    , plotdata : Series
    , insertion_dates : Array String
    , date_index : Int
    -- user meta edition
    , metaitem : (String, String)
    }


type Msg
    = GotMeta (Result Http.Error String)
    | GetPermissions (Result Http.Error String)
    | GotFormula (Result Http.Error String)
    | CodeHighlight (Result Http.Error String)
    | Components (Result Http.Error String)
    | GotLog (Result Http.Error String)
    | GotPlotData (Result Http.Error String)
    | InsertionDates (Result Http.Error String)
    | ToggleExpansion
    | ChangedIdate String
    -- metadata edition
    | MetaEditAsked
    | MetaItemToDelete String
    | EditedValue String
    | EditedKey String
    | AddMetaItem
    | SaveMeta
    | MetaSaved (Result Http.Error String)


decodelogentry : D.Decoder Logentry
decodelogentry =
    D.map4 Logentry
        (D.field "rev" D.int)
        (D.field "author" D.string)
        (D.field "date" D.string)
        (D.field "meta" (D.dict M.decodemetaval))


decodelog : D.Decoder (List Logentry)
decodelog =
    D.list decodelogentry


decodeidates : D.Decoder (List String)
decodeidates =
    D.list D.string


getwriteperms urlprefix =
    Http.get
        { expect = Http.expectString GetPermissions
        , url = UB.crossOrigin urlprefix [ "tsinfo", "canwrite" ] []
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


getlog : String -> String-> Cmd Msg
getlog urlprefix name  =
    Http.get
        { expect = Http.expectString GotLog
        , url = UB.crossOrigin urlprefix
              [ "api", "series", "log" ]
              [ UB.string "name" name
              , UB.int "limit" 10 ]
        }


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


getcomponents model =
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

getidates model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "tsinfo", "idates" ]
              [ UB.string "name" model.name ]
        , expect = Http.expectString InsertionDates
        }


savemeta model =
    Http.request
        { method = "PUT"
        , body = Http.jsonBody <| E.object
                 [ ("name", E.string model.name )
                 , ("metadata" , E.string <| M.encodemeta model.usermeta )
                 ]
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url =
              UB.crossOrigin
              model.baseurl
              [ "api", "series", "metadata" ] [ ]
        , expect = Http.expectString MetaSaved
        }


unwraperror : Http.Error -> String
unwraperror resp =
    case resp of
        Http.BadUrl x -> "bad url: " ++ x
        Http.Timeout -> "the query timed out"
        Http.NetworkError -> "there was a network error"
        Http.BadStatus val -> "we got a bad status answer: " ++ String.fromInt val
        Http.BadBody body -> "we got a bad body: " ++ body


adderror model error =
    { model | errors = List.append model.errors [error] }


nocmd model = ( model, Cmd.none )


first = Tuple.first
snd = Tuple.second


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doerr error =
            nocmd <| adderror model error
    in
    case msg of
        GotMeta (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    let
                        stdmeta = Dict.filter (\k v -> (List.member k M.metanames)) allmeta
                        usermeta = Dict.filter (\k v -> not (List.member k M.metanames)) allmeta
                        newmodel =
                            { model
                                | meta = stdmeta
                                , usermeta = usermeta
                            }
                        next = getidates model
                        cmd = if supervision newmodel == "formula"
                              then Cmd.batch [ getformula model, next ]
                              else Cmd.batch [ getlog model.baseurl model.name, next ]
                    in ( newmodel, cmd )
                Err err ->
                    doerr <| D.errorToString err

        GotMeta (Err err) ->
            doerr  <| unwraperror err

        GetPermissions (Ok rawperm) ->
            case D.decodeString D.bool rawperm of
                Ok perms ->
                   nocmd { model | canwrite = perms }
                Err err ->
                    doerr <| D.errorToString err

        GetPermissions (Err err) ->
            doerr  <| unwraperror err

        GotPlotData (Ok rawdata) ->
            case D.decodeString seriesdecoder rawdata of
                Ok val ->
                    nocmd { model | plotdata = val }
                Err err ->
                    doerr <| D.errorToString err

        GotPlotData (Err err) ->
            doerr <|unwraperror err

        GotFormula (Ok rawformula) ->
            case D.decodeString D.string rawformula of
                Ok formula ->
                    ( model
                    , Cmd.batch [ pygmentyze model formula
                                , getcomponents model
                                ]
                    )
                Err err ->
                    doerr <| D.errorToString err

        GotFormula (Err error) ->
            doerr  <| unwraperror error

        CodeHighlight (Ok rawformula) ->
            case D.decodeString D.string rawformula of
                Ok formula ->
                    nocmd { model | formula = Just formula }
                Err err ->
                    doerr <| D.errorToString err

        CodeHighlight (Err error) ->
            doerr <| unwraperror error

        Components (Ok rawcomponents) ->
            case D.decodeString (D.keyValuePairs D.string) rawcomponents of
                Ok components ->
                    nocmd { model | formula_components = components }
                Err err ->
                    doerr <| D.errorToString err

        Components (Err error) ->
            doerr <| unwraperror error

        GotLog (Ok rawlog) ->
            case D.decodeString decodelog rawlog of
                Ok log ->
                    nocmd { model | log = log }
                Err err ->
                    doerr <| D.errorToString err

        GotLog (Err error) ->
            doerr <| unwraperror error

        InsertionDates (Ok rawdates) ->
            case D.decodeString decodeidates rawdates of
                Ok dates ->
                    nocmd { model
                              | insertion_dates = Array.fromList dates
                              , date_index = List.length dates - 1
                          }
                Err err ->
                    doerr <| D.errorToString err

        InsertionDates (Err error) ->
            doerr <| unwraperror error

        ToggleExpansion ->
            let
                state = model.formula_expanded
            in
                ( { model | formula_expanded = not state }
                , getformula { model | formula_expanded = not state }
                )

        ChangedIdate strindex ->
            let
                index = Maybe.withDefault
                       model.date_index -- keep current
                       (String.toInt strindex)
            in
            case Array.get index model.insertion_dates of
                Nothing -> ( model, Cmd.none )
                Just date ->
                    ( { model | date_index = index }
                    , getplotdata model.baseurl model.name (Just date) GotPlotData
                    )

        -- user metadata edition

        MetaEditAsked ->
            nocmd { model | editing = not model.editing }

        MetaItemToDelete key ->
            nocmd { model | usermeta = Dict.remove key model.usermeta }

        EditedKey key ->
            nocmd { model | metaitem = ( key, Tuple.second model.metaitem ) }

        EditedValue val ->
            nocmd { model | metaitem = ( Tuple.first model.metaitem, val ) }

        AddMetaItem ->
            -- eat the metaitems
            let
                newmeta = Dict.insert
                          (Tuple.first model.metaitem)
                          (M.MString <| Tuple.second model.metaitem)
                          model.usermeta
            in
            ( { model
                  | metaitem = ("", "")
                  , usermeta = newmeta
              }
            , Cmd.none
            )

        SaveMeta ->
            ( model
            , savemeta model
            )

        MetaSaved (Ok _) ->
            nocmd { model
                  | editing = False
                  , metaitem = ("", "")
                  }

        MetaSaved (Err err) ->
            doerr <| unwraperror err


supervision model =
    case Dict.get "supervision_status" model.meta of
        Nothing -> "formula"
        Just x -> M.metavaltostring x


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
            first ab |> (++) " → " |> (++) (M.metavaltostring <| snd ab)
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
        Just something -> M.metavaltostring something


viewmeta model =
    let
        hidden = ["index_names", "index_type", "index_dtype", "value_dtype"]
        fixval name val =
            if name == "supervision_status" && val == ""
            then "formula"
            else val
        elt name =
            li [] [text (name ++ " → " ++ (fixval name <| (dget name model.meta)))]
    in
    div []
    [ h2 [] [text "Metadata"]
    , ul [] <| List.map elt <| List.filter (\x -> not <| List.member x hidden) M.metanames
    ]


viewusermetaheader model =
    let
        editaction =
            if model.canwrite && not model.editing then
                button
                [ A.attribute "type" "button"
                , A.class "btn btn-primary"
                , onClick MetaEditAsked
                ]
                [text "edit"]
            else span [] []
    in
        h2  []
            [ text "User Metadata"
            , span [] [text " "]
            , editaction
            ]


viewusermeta model =
    if model.editing then editusermeta model else
    let
        elt (k, v) =
            li [] [text <| (k ++ " → " ++ (M.metavaltostring v))]
    in
    div []
        [ viewusermetaheader model
        , ul [] (List.map elt (Dict.toList model.usermeta))
        ]


editusermeta model =
    let
        deletefields key val =
            div [ A.class "form-row" ]
                [ div [ A.class "col-3" ]
                      [ input
                            [ A.attribute "type" "text"
                            , A.class "form-control"
                            , A.disabled True
                            , A.value key
                            ] []
                      ]
                , div [ A.class "col-6" ]
                    [ input [ A.attribute "type" "text"
                            , A.class "form-control"
                            , A.placeholder "value"
                            , A.value <| val
                            ] []
                    ]
                , div [A.class "col" ]
                      [ button
                            [ A.attribute "type" "button"
                            , A.class "btn btn-warning"
                            , onClick (MetaItemToDelete key)
                            ]
                            [ text "delete" ]
                      ]
                ]
        addfields key val =
            div [ A.class "form-row" ]
                [ div [ A.class "col-3" ]
                      [ input
                            [ A.attribute "type" "text"
                            , A.class "form-control"
                            , A.placeholder "key"
                            , A.value key
                            , onInput EditedKey
                            ] []
                      ]
                , div [ A.class "col-6" ]
                    [ input [ A.attribute "type" "text"
                            , A.class "form-control"
                            , A.placeholder "value"
                            , A.value <| val
                            , onInput EditedValue
                            ] []
                    ]
                ]
        editfields ab = deletefields (Tuple.first ab) (M.metavaltostring <| Tuple.second ab)
    in
    div []
        [ viewusermetaheader model
        , form
              [ onSubmit SaveMeta ]
              <| (List.map editfields (Dict.toList model.usermeta)) ++
                  if not <| Dict.isEmpty model.usermeta then
                      [ button
                            [ A.attribute "type" "submit"
                            , A.class "btn btn-primary col-sm-10"
                            ]
                            [ text "save entries"]
                      ] else []
        , form [ onSubmit AddMetaItem ]
            [ addfields (first model.metaitem) (snd model.metaitem)
            , button
                  [ A.attribute "type" "submit"
                  , A.class "btn btn-primary col-sm-10"
                  ]
                  [ text "add entry"]
            ]
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
        , div [] (List.map (\x -> p [] [text x]) model.errors)
        ]
    else span [] []


viewdatesrange model =
    let
        numidates = Array.length model.insertion_dates
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> date
    in
    if numidates < 2
    then div [] []
    else div []
        [ input
              [ A.attribute "type" "range"
              , A.min "0"
              , A.max (String.fromInt numidates)
              , A.value (String.fromInt model.date_index)
              , A.class "form-control-range"
              , A.title currdate
              , onInput ChangedIdate
              ] []
        ]


viewplot model =
    let
        plot = scatterplot model.name
               (Dict.keys model.plotdata)
               (Dict.values model.plotdata)
               "lines"
        args = plotargs "plot" [plot]
    in
    div []
        [ h2 [] [ text "Plot" ]
        , viewdatesrange model
        , div [ A.id "plot" ] []
        -- the "plot-figure" node is pre-built in the template side
        -- (html component)
        , node "plot-figure" [ A.attribute "args" args ] []
        ]


view : Model -> Html Msg
view model =
    div []
        [h1 [] [text ("Series " ++ model.name)]
        , viewmeta model
        , viewusermeta model
        , viewformula model
        , viewlog model
        , viewcomponents model
        , viewplot model
        , viewerrors model
        ]


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
                     False
                     False
                     []
                     Dict.empty
                     Dict.empty
                     False
                     Nothing
                     []
                     []
                     Dict.empty
                     Array.empty
                     0
                     ("", "")
               ,
                   Cmd.batch
                       [ M.getmetadata input.baseurl input.name GotMeta
                       , getplotdata input.baseurl input.name Nothing GotPlotData
                       , getwriteperms input.baseurl
                       ]
               )
           sub model = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
