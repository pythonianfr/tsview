module Groupinfo exposing (main)

import Array exposing (Array)
import Browser
import Debouncer.Messages as Debouncer exposing
    (Debouncer
    , fromSeconds
    , provideInput
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import Either exposing (Either)
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Info as I
import Json.Decode as D
import Json.Encode as E
import JsonTree as JT exposing (TaggedValue(..))
import Metadata as M
import Plotter exposing
    ( getgroupplotdata
    , groupdecoder
    , scatterplot
    , plotargs
    , Group
    )
import Url.Builder as UB
import Util as U


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
    , expanded_formula : Maybe String
    , formula_components : Maybe JT.Node
    , expanded_formula_components : Maybe JT.Node
    -- log
    , log : List Logentry
    -- plot
    , plotdata : Maybe Group
    , insertion_dates : Array String
    , mindate : String
    , maxdate : String
    , date_index : Int
    , date_index_deb : Debouncer Msg
    -- user meta edition
    , metaitem : (String, String)
    , editeditems : Dict String String
    }


type Msg
    = GotMeta (Result Http.Error String)
    | GetPermissions (Result Http.Error String)
    | GotPlotData (Result Http.Error String)
    -- dates
    | ChangedIdate String
    | DebounceChangedIdate (Debouncer.Msg Msg)
    | IdatePickerChanged String
    | FvdatePickerChanged String
    | TvdatePickerChanged String
    -- formula
    | GotFormula (Result Http.Error String)
    | CodeHighlight (Result Http.Error String)
    | Components (Result Http.Error String)
    | InsertionDates (Result Http.Error String)
    | ToggleExpansion
    -- metadata edition
    | MetaEditAsked
    | MetaEditCancel
    | MetaItemToDelete String
    | EditedValue String String
    | NewValue String
    | NewKey String
    | AddMetaItem
    | SaveMeta
    | MetaSaved (Result Http.Error String)



logentrydecoder : D.Decoder Logentry
logentrydecoder =
    D.map4 Logentry
        (D.field "rev" D.int)
        (D.field "author" D.string)
        (D.field "date" D.string)
        (D.field "meta" (D.dict M.decodemetaval))


logdecoder : D.Decoder (List Logentry)
logdecoder =
    D.list logentrydecoder


getplot model atidate =
    let
        idate =
            Array.get model.date_index model.insertion_dates
    in
        getgroupplotdata model.baseurl model.name
            (if atidate then idate else Nothing)
            GotPlotData
            model.mindate
            model.maxdate


getidates model =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", "group", "insertion_dates" ]
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
              [ "api", "group", "metadata" ] [ ]
        , expect = Http.expectString MetaSaved
        }


updatedchangedidatebouncer =
    { mapMsg = DebounceChangedIdate
    , getDebouncer = .date_index_deb
    , setDebouncer = \deb model -> { model | date_index_deb = deb }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
    in
    case msg of
        GotMeta (Ok result) ->
            case D.decodeString M.decodemeta result of
                Ok allmeta ->
                    let
                        (stdmeta, usermeta) =
                            Dict.partition (\k v -> (List.member k M.metanames)) allmeta
                        newmodel =
                            { model
                                | meta = stdmeta
                                , usermeta = usermeta
                            }
                        next = getidates model
                        cmd = Cmd.batch [ I.getformula model "group" GotFormula, next ]
                    in ( newmodel, cmd )
                Err err ->
                    doerr "gotmeta decode" <| D.errorToString err

        GotMeta (Err err) ->
            doerr "gotmeta http"  <| U.unwraperror err

        GetPermissions (Ok rawperm) ->
            case D.decodeString D.bool rawperm of
                Ok perms ->
                   U.nocmd { model | canwrite = perms }
                Err err ->
                    doerr "getpermissions decode" <| D.errorToString err

        GetPermissions (Err err) ->
            doerr "getpermissions http" <| U.unwraperror err

        GotPlotData (Ok rawdata) ->
            case D.decodeString groupdecoder rawdata of
                Ok val ->
                    let
                        dates = Dict.keys val
                        minappdate =
                            case dates of
                                head::_ -> U.cleanupdate head
                                []  -> ""
                        maxappdate = U.cleanupdate <| Maybe.withDefault "" <| List.maximum dates
                        newmodel =
                            case model.plotdata of
                                Nothing ->
                                    { model
                                        | plotdata = Just val
                                        , mindate = U.dateof minappdate
                                        , maxdate = U.dateof maxappdate
                                    }
                                Just data -> { model | plotdata = Just val }
                    in
                    U.nocmd newmodel
                Err err ->
                    doerr "gotplotdata decode" <| D.errorToString err

        GotPlotData (Err err) ->
            doerr "gotplotdata error" <| U.unwraperror err

        GotFormula (Ok rawformula) ->
            case D.decodeString D.string rawformula of
                Ok formula ->
                    ( model
                    , Cmd.batch [ U.pygmentyze model formula CodeHighlight
                                -- , getcomponents model
                                ]
                    )
                Err _ ->
                    -- there is no formula -> there might be logs !
                    -- but right now we don't have them anyway
                    U.nocmd model

        GotFormula (Err error) ->
            doerr "gotformula http" <| U.unwraperror error

        -- code

        CodeHighlight (Ok rawformula) ->
            case D.decodeString D.string rawformula of
                Ok formula ->
                    case model.formula_expanded of
                        True ->
                            U.nocmd { model | expanded_formula = Just formula }
                        False ->
                            U.nocmd { model | formula = Just formula }
                Err err ->
                    doerr "codehightlight decode" <| D.errorToString err

        CodeHighlight (Err error) ->
            doerr "codehighlight http" <| U.unwraperror error

        -- components

        Components (Ok rawcomponents) ->
            case JT.parseString rawcomponents of
                Ok components ->
                    case model.formula_expanded of
                        True ->
                            U.nocmd { model | expanded_formula_components = Just components }
                        False ->
                            U.nocmd { model | formula_components = Just components }
                Err err ->
                    doerr "components decode" <| D.errorToString err

        Components (Err error) ->
            doerr "components http" <| U.unwraperror error

        InsertionDates (Ok rawdates) ->
            case D.decodeString I.idatesdecoder rawdates of
                Ok dates ->
                    U.nocmd { model
                                | insertion_dates = Array.fromList dates
                                , date_index = List.length dates - 1
                            }
                Err err ->
                    doerr "idates decode" <| D.errorToString err

        InsertionDates (Err error) ->
            doerr "idates http" <| U.unwraperror error

        ToggleExpansion ->
            let
                state = model.formula_expanded
            in
                ( { model | formula_expanded = not state }
                , case model.expanded_formula of
                      Nothing ->
                          I.getformula { model | formula_expanded = not state } "group" GotFormula
                      Just _ ->
                          Cmd.none
                )

        DebounceChangedIdate val ->
            Debouncer.update update updatedchangedidatebouncer val model


        ChangedIdate strindex ->
            let
                index = Maybe.withDefault
                       model.date_index -- keep current
                       (String.toInt strindex)
                newmodel = { model | date_index = index }
            in
            case Array.get index model.insertion_dates of
                Nothing -> ( model, Cmd.none )
                Just date ->
                    ( newmodel
                    , getplot newmodel True
                    )

        IdatePickerChanged value ->
            let
                comparedates d1 d2 =
                    d1 > d2
                newarray =  Array.filter (comparedates value) <|
                            Array.map U.cleanupdate model.insertion_dates
                newindex = max 0 <| Array.length newarray - 1
                newmodel = { model | date_index = newindex }
            in ( newmodel
               , getplot newmodel True
               )

        FvdatePickerChanged value ->
            let
                newmodel = { model | mindate = value }
            in
                ( newmodel
                , getplot newmodel True
                )

        TvdatePickerChanged value ->
            let
                newmodel = { model | maxdate = value }
            in
                ( newmodel
                , getplot newmodel True
                )

        -- user metadata edition

        MetaEditAsked ->
            U.nocmd { model
                        | editing = True
                        , editeditems = Dict.map (\k v -> M.metavaltostring v) model.usermeta
                    }

        MetaEditCancel ->
            U.nocmd { model
                        | editing = False
                        , editeditems = Dict.empty
                        , metaitem = ("", "")
                    }

        MetaItemToDelete key ->
            U.nocmd { model | editeditems = Dict.remove key model.editeditems }

        EditedValue key value ->
            U.nocmd { model | editeditems = Dict.insert key value model.editeditems }

        NewKey key ->
            U.nocmd { model | metaitem = ( key, Tuple.second model.metaitem ) }

        NewValue val ->
            U.nocmd { model | metaitem = ( U.first model.metaitem, val ) }

        AddMetaItem ->
            -- eat the metaitems
            if (U.first model.metaitem == "") || (U.snd model.metaitem == "")
            then U.nocmd model else
            let
                edited = Dict.insert
                         (U.first model.metaitem)
                         (U.snd model.metaitem)
                         model.editeditems
            in
            ( { model
                  | metaitem = ("", "")
                  , editeditems = edited
              }
            , Cmd.none
            )

        SaveMeta ->
            let
                decode rawitem =
                    case D.decodeString M.decodemetaval rawitem of
                        Ok item -> item
                        Err err ->
                            -- form strings are not json strings
                            -- this is why plain string parsing will fail ...
                            M.MString rawitem
                newmodel = { model | usermeta = Dict.map (\k v -> decode v) model.editeditems }
            in
            ( newmodel, savemeta newmodel )

        MetaSaved (Ok _) ->
            U.nocmd { model
                        | editing = False
                        , editeditems = Dict.empty
                        , metaitem = ("", "")
                    }

        MetaSaved (Err err) ->
            doerr "metasaved http" <| U.unwraperror err


-- views

metadicttostring d =
    let
        builditem ab =
            U.first ab ++ " → " ++ (M.metavaltostring <| U.snd ab)
    in
    String.join "," <| List.map builditem (Dict.toList d)


viewlogentry entry =
    tr [ ]
        [ th [ A.scope "row" ] [ text (String.fromInt entry.rev) ]
        , td [ ] [ text entry.author ]
        , td [ ] [ text entry.date ]
        , td [ ] [ text <| metadicttostring entry.meta ]
        ]


viewlog model showtitle =
    if List.length model.log > 0 then
        div [ ]
            [ if showtitle
              then h2 [ ] [ text "History Log" ]
              else span [] []
            , table [A.class "table table-striped table-hover table-sm"]
                [ thead [ ]
                      [ td [ A.scope "col" ] [ text "#" ]
                      , td [ A.scope "col" ] [ text "author" ]
                      , td [ A.scope "col" ] [ text "date" ]
                      , td [ A.scope "col" ] [ text "meta" ]
                      ]
                , tbody [ ] <| List.map viewlogentry (List.reverse model.log)
                ]
            ]
    else div [ ] [ ]


metaevents =
    { metaeditasked = MetaEditAsked
    , metaeditcancel = MetaEditCancel
    , editedvalue = EditedValue
    , metaitemtodelete = MetaItemToDelete
    , newkey = NewKey
    , newvalue = NewValue
    , savemeta = SaveMeta
    , addmetaitem = AddMetaItem
    }


viewidatepicker model =
    let
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> U.cleanupdate date
    in div
        [ ]
        [ label [ A.for "idate-picker" ] [ text "revision date" ]
        , span [ ] [ text " " ]
        , input [ A.type_ "datetime-local"
                , A.id "idate-picker"
                , A.name "idate-picker"
                , A.value currdate
                , onInput IdatePickerChanged
                ] [ ]
        , span [ ] [ text " " ]
        , label [ A.for "fvd-picker" ] [ text "from value date" ]
        , span [ ] [ text " " ]
        , input [ A.type_ "date"
                , A.id "fvd-picker"
                , A.name "fvd-picker"
                , A.value model.mindate
                , onInput FvdatePickerChanged
                ] [ ]
        , span [ ] [ text " " ]
        , label [ A.for "tvd-picker" ] [ text "to value date" ]
        , span [ ] [ text " " ]
        , input [ A.type_ "date"
                , A.id "tvd-picker"
                , A.name "tvd-picker"
                , A.value model.maxdate
                , onInput TvdatePickerChanged
                ] [ ]
        ]


viewdatesrange model =
    let
        numidates = Array.length model.insertion_dates
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> date
    in
    if numidates < 2
    then div [ ] [ ]
    else
        Html.map (provideInput >> DebounceChangedIdate) <|
            div [ ]
            [ input
                  [ A.attribute "type" "range"
                  , A.min "0"
                  , A.max (String.fromInt (numidates - 1))
                  , A.value (String.fromInt model.date_index)
                  , A.class "form-control-range"
                  , A.title currdate
                  , onInput ChangedIdate
                  ] [ ]
            ]


viewplot model =
    let
        groupdata =
            case model.plotdata of
                Nothing -> Dict.empty
                Just data -> data

        plot (name, plotdata) =
            scatterplot
                name
                (Dict.keys plotdata)
                (Dict.values plotdata)
                "lines"

        plots = List.map plot <| Dict.toList groupdata

        args =
            plotargs "plot" plots
    in
    div [ ]
        [ h2 [ ] [ text "Plot" ]
        , viewidatepicker model
        , viewdatesrange model
        , div [ A.id "plot" ] [ ]
        -- the "plot-figure" node is pre-built in the template side
        -- (html component)
        , node "plot-figure" [ A.attribute "args" args ] [ ]
        ]


view : Model -> Html Msg
view model =
    div [ A.style "margin" ".5em" ]
        [ h1 [ ]
              [ text "Series "
              , span
                    [ A.class "font-italic" ]
                    [ text model.name ]
              ]
        -- , viewseealso model
        , I.viewmeta model
        , I.viewusermeta model metaevents
        , I.viewformula model ToggleExpansion
        , case model.formula of
              Nothing -> viewlog model True
              Just _ -> span [] []
        -- , I.viewcomponents model
        , viewplot model
        , I.viewerrors model
        ]


type alias Input =
    { baseurl : String
    , name : String
    }


main : Program Input  Model Msg
main =
       let
           debouncerconfig =
               Debouncer.manual
                   |> settleWhenQuietFor (Just <| fromSeconds 0.015)
                   |> toDebouncer

           init input =
               let
                   model = Model
                           input.baseurl
                           input.name
                           -- metadata edition
                           False
                           False
                           -- all errors
                           [ ]
                           -- metadata
                           Dict.empty
                           Dict.empty
                           -- formula
                           False
                           Nothing
                           Nothing
                           Nothing
                           Nothing
                           -- log
                           [ ]
                           -- plot
                           Nothing
                           Array.empty
                           ""
                           ""
                           0
                           debouncerconfig
                           -- user meta edittion
                           ("", "")
                           Dict.empty
               in
               ( model
               , Cmd.batch
                   [ M.getmetadata input.baseurl input.name GotMeta "group"
                   , getplot model False
                   , I.getwriteperms input.baseurl GetPermissions
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
