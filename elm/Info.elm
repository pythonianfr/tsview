module Info exposing
    ( delete
    , DataType(..)
    , formuladecoder
    , getformula
    , getidates
    , getlog
    , getoldformulas
    , getoldmetadata
    , getwriteperms
    , GroupType(..)
    , idatesdecoder
    , Logentry
    , logdecoder
    , metatype
    , msgdoesnotexist
    , OldFormula
    , oldformulasdecoder
    , OldMetadata
    , oldmetasdecoder
    , rename
    , savemeta
    , showformula
    , SeriesType(..)
    , Direction(..)
    , viewactionwidgets
    , viewdatespicker
    , viewDatesRange
    , viewdeletion
    , viewerrors
    , viewformula
    , viewgraph
    , viewHoverGraph
    , viewHistoryGraph
    , viewlog
    , viewoldformulas
    , viewrenameaction
    , viewtitle
    , viewusermeta
    , viewWidgetIdates
    )


import Array exposing (Array)
import Debouncer.Messages exposing (provideInput)
import Dict exposing (Dict)
import Horizon exposing
    ( horizonview
    , HorizonModel
    )
import Horizon as ModuleHorizon
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Json.Encode as E
import JsonTree as JT exposing (TaggedValue(..))
import Lisp
import List.Extra as List
import Plotter exposing
    ( Trace
    , TraceOptions
    , defaultLayoutOptions
    , defaultConfigOptions
    , defaultTraceOptions
    , serializedPlotArgs
    , scatterplot
    )
import Metadata as M
import Url.Builder as UB
import Util as U


type SeriesType
    = Primary
    | Formula


type GroupType
    = GroupPrimary
    | GroupFormula
    | GroupBound


type DataType
    = SeriesType SeriesType
    | GroupType GroupType


type Direction
    = Prev
    | Next


type Position
    = Left
    | Center
    | Right


getwriteperms urlprefix event =
    Http.get
        { expect = Http.expectString event
        , url = UB.crossOrigin urlprefix [ "canwrite" ] [ ]
        }


type alias Logentry =
    { rev : Int
    , author : String
    , date : String
    , meta : M.Metadata
    }


type alias FormulaResponse =
    { level : Int
    , formula : String
    }


cleandate strdate =
    String.replace
        "T"
        " "
        ( String.left 19 strdate ) ++ " " ++ ( String.right 6 strdate )


formuladecoder =
    D.map2 FormulaResponse
        (D.field "level" D.int)
        (D.field "formula" D.string)


getformula model name depth dtype callback  =
    Http.get
        { expect = Http.expectString callback
        , url =
            UB.crossOrigin model.baseurl
                [ "api", dtype, "formula" ]
                [ UB.string "name" name
                , UB.int "display" 1
                , UB.int "level" depth
                ]
        }


idatesdecoder : D.Decoder (List String)
idatesdecoder =
    D.field "insertion_dates" (D.list D.string)


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


getidates model dtype callback viewnocache =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", dtype, "insertion_dates" ]
              [ UB.string "name" model.name
              , UB.int "nocache" <| U.bool2int viewnocache
              ]
        , expect = Http.expectString callback
        }


delete model dtype event =
    Http.request
        { method = "delete"
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        , url =
            UB.crossOrigin
                model.baseurl
                [ "api", dtype, "state" ]
                [ UB.string "name" model.name ]
        , body = Http.emptyBody
        , expect = Http.expectString event
        }


rename model newname dtype event =
    Http.request
        { method = "put"
        , body = Http.jsonBody <| E.object
                 [ ( "name", E.string model.name )
                 , ( "newname", E.string newname )
                 ]
        , url =
            UB.crossOrigin model.baseurl [ "api", dtype, "state" ] [ ]
        , expect = Http.expectString event
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


savemeta model dtype callback =
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
              [ "api", dtype, "metadata" ] [ ]
        , expect = Http.expectString callback
        }


getlog urlprefix name logLimit dtype callback =
    Http.get
        { expect = Http.expectString callback
        , url = UB.crossOrigin urlprefix
              [ "api", dtype, "log" ]
              [ UB.string "name" name
              , UB.int "limit" (Maybe.withDefault 10 logLimit)
              ]
        }


type alias OldMetadata =
    { stamp : String
    , metadata : M.Metadata
    , user : String
    }


oldmetasdecoder =
    let
        decoder =
            D.map3 OldMetadata
                (D.index 0 D.string)
                (D.index 1 M.decodemeta)
                (D.index 2 D.string)
    in
    D.list decoder


getoldmetadata baseurl name callback dtype =
    Http.get
        { expect = Http.expectString callback
        , url = UB.crossOrigin baseurl
              [ "api", dtype, "metadata" ]
              [ UB.string "name" name
              , UB.string "type" "archive"
              ]
        }


type alias OldFormula =
    { formula : String
    , stamp : String
    , tz : String
    , user : String
    }


oldformulasdecoder =
    let
        decoder =
            D.map4 OldFormula
                (D.index 0 D.string)
                (D.index 1 D.string)
                (D.index 2 D.string)
                (D.index 3 D.string)
    in
    D.list decoder


getoldformulas baseurl name callback dtype =
    Http.get
        { expect = Http.expectString callback
        , url = UB.crossOrigin baseurl
              [ "api", dtype, "old_formulas" ]
              [ UB.string "name" name ]
        }


metatype val =
    case val of
        Nothing -> "virt"
        Just x ->
            case x of
                M.MString _ -> "str"
                M.MInt _ -> "int"
                M.MFloat _ -> "float"
                M.MBool _ -> "bool"
                M.MList _ -> "list"
                M.Mnull _ -> "null"


viewerrors model =
    if List.length model.errors > 0 then
    H.div [ ]
        [ H.h2 [ ] [ H.text "Errors" ]
        , H.div [ ] <| List.map (\x -> H.p [ ] [ H.text x ]) model.errors
        ]
    else H.span [ ] [ ]


tzawareseries: { a | meta: Dict String M.MetaVal } -> Bool
tzawareseries model =
    (M.dget "tzaware" model.meta) == "true"


type alias PartialModel a =
    { a | name: String
        , canwrite: Bool 
        , horizon: HorizonModel
        , baseurl: String
        , meta: Dict String M.MetaVal
        , tzaware: Bool
    }


viewactionwidgets: PartialModel a -> DataType -> (ModuleHorizon.Msg -> msg) -> Maybe (H.Html msg) -> Bool -> String -> Maybe (String, String) -> List ( H.Html msg )
viewactionwidgets model datatype convertmsg permalink editor pagetitle bounds =
    let
        editorlabel =
            case datatype of
                SeriesType Primary ->
                    if editor && model.canwrite then "edit values" else "view values"
                SeriesType Formula ->
                    "show values"
                _ ->
                    ""
        queryParameters =
            case bounds of
                Nothing ->
                    [ UB.string "name" model.name ]
                Just ( min, max ) ->
                    if editor
                    then  [ UB.string "name" model.name
                          , UB.string "startdate" min
                          , UB.string "enddate" max
                          ]
                    else [ UB.string "name" model.name ]
    in
    [ H.div
          [ HA.class "page-title" ]
          [ H.text pagetitle ]
    , horizonview
        model.horizon
        convertmsg
        "action-center"
        model.tzaware
    , H.div
        [ HA.class "action-right" ]
        ( case permalink of
            Nothing -> []
            Just link -> [ link ]
        )
    , H.div [ HA.class "action-right" ]
        [ H.a [ HA.href <| UB.crossOrigin model.baseurl
                    [ if editor then "tseditor" else "tsinfo" ]
                    queryParameters
              ]
              [ H.text <| if editor then editorlabel else "series info" ]
        , case datatype of
              SeriesType Formula ->
                  H.a [ HA.href <| UB.crossOrigin model.baseurl [ "tsformula" ]
                            [ UB.string "name" model.name ]
                      ]
                      [ H.text "edit formula" ]
              GroupType GroupFormula ->
                  H.a [ HA.href <| UB.crossOrigin model.baseurl [ "tsformula-group" ]
                            [ UB.string "name" model.name ]
                      ]
                      [ H.text "edit formula" ]
              _ ->
                  H.span [ ] [ ]
        ]
    ]


viewtitle model copyclass copyevent =
    let
        ( tzaware, tzbadge, tztitle ) =
            if tzawareseries model
            then ( "tzaware"
                 , "badge-success"
                 , "This series is time zone aware."
                 )
            else ( "tznaive"
                 , "badge-warning"
                 , "This series is not associated with a time zone."
                 )

        supervision =
            M.dget "supervision_status" model.meta
    in
    H.p
        [ HA.class "series-info" ]
        [ H.i
              [ HA.class copyclass
              , HE.onClick copyevent
              ] [ ]
        , H.span
            [ HA.class "badges-spacing" ]
            [ H.span
                  [ HA.class "font-italic h4" ]
                  [ H.text <| " " ++ model.name ++ " " ]
            , H.span
                [ HA.class "badge h4"
                , HA.class tzbadge
                , HA.title tztitle
                ]
                [ H.text tzaware ]
            , H.span
                [ HA.class "badge badge-info h4"
                , HA.title "Supervision status of the series."
                ]
                [ H.text supervision ]
            , H.span
                [ HA.class "badge badge-secondary h4"
                , HA.title "Name of the series source."
                ]
                [ H.text model.source ]
            ]
        ]


viewdatespicker model events =
    let
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> U.cleanupdate date

        idate =
            [ H.label [ HA.for "idate-picker" ] [ H.text "Revision date" ]
            , H.span [ ] [ H.text " " ]
            , H.input [ HA.type_ "datetime-local"
                      , HA.id "idate-picker"
                      , HA.name "idate-picker"
                      , HA.value currdate
                      , HE.onInput events.idatepickerchanged
                      ] [ ]
            ]

        fvdate =
            [ H.label [ HA.for "fvd-picker" ] [ H.text "from value date" ]
            , H.span [ ] [ H.text (" : " ++ model.mindate) ]
            ]

        tvdate =
            [ H.label [ HA.for "tvd-picker" ] [ H.text "to value date" ]
            , H.span [ ] [ H.text (" : " ++ model.maxdate) ]
            ]

        spacer =
            [ H.span [ ] [ H.text " " ] ]

    in H.div
        [ ]
        (idate ++ spacer ++ fvdate ++ spacer ++ tvdate)


viewusermetaheader model events showtitle =
    let
        editaction =
            if model.canwrite then
                if not model.editing then
                    H.button
                    [ HA.attribute "type" "button"
                    , HA.class "btn btn-primary"
                    , HE.onClick events.metaeditasked
                    ] [ H.text "edit" ]
                else
                    H.button
                    [ HA.attribute "type" "button"
                    , HA.class "btn btn-warning"
                    , HE.onClick events.metaeditcancel
                    ] [ H.text "cancel" ]
            else H.span [ ] [ ]
    in
    if showtitle
    then H.h2  [ ]
        [ H.text "User Metadata"
        , H.span [ ] [ H.text " "]
        , editaction
        ]
    else H.div [ ]
        [ editaction ]


viewusermeta model events showtitle =
    if model.editing then editusermeta model events showtitle else
    let
        elt (k, v) =
            H.li [ ] [ H.text <| k
                           ++ " → "
                           ++ (M.metavaltostring v)
                           ++ " ["
                           ++ (metatype <| Just v)
                           ++ "]"
                   ]

        showmeta =
            if not <| Dict.isEmpty model.usermeta then
                H.div [ ]
                    [ viewusermetaheader model events showtitle
                    , H.ul [ ] <| List.map elt (Dict.toList model.usermeta)
                    ]
            else
                H.div [ ]
                    [ viewusermetaheader model events showtitle
                    , H.text "No user-defined metadata yet."
                    ]

        showoldmetas =
            let
                renderitem item =
                    H.tr [ ]
                        [ H.td [ ] [ H.text <| cleandate item.stamp ]
                        , H.td [ ] <| List.map elt <| Dict.toList item.metadata
                        , H.td [ ] [ H.text item.user ]
                        ]

                renderall =
                    H.table [ HA.class "table w-auto" ]
                        [ H.thead [ ]
                              [ H.tr [ ]
                                    [ H.td [ ] [ H.text "Archived at" ]
                                    , H.td [ ] [ H.text "Metadata" ]
                                    , H.td [ ] [ H.text "User" ]
                                    ]
                              ]
                        , H.tbody [ ]
                            <| List.map renderitem model.metahistory
                        ]

            in
            if List.length model.metahistory > 0 then renderall else
                H.span [ ] [ ]

    in
    H.div [ ]
        [ showmeta
        , showoldmetas
        ]


editusermeta model events showtitle =
    let
        deletefields key val =
            H.div [ HA.class "form-row" ]
                [ H.div [ HA.class "col-3" ]
                      [ H.input
                            [ HA.attribute "type" "text"
                            , HA.class "form-control"
                            , HA.disabled True
                            , HA.value key
                            ] [ ]
                      ]
                , H.div [ HA.class "col-6" ]
                    [ H.input [ HA.attribute "type" "text"
                              , HA.class "form-control"
                              , HA.placeholder "value"
                              , HA.value val
                              , HE.onInput <| events.editedvalue key
                              ] [ ]
                    ]
                , H.div [ HA.class "col" ]
                    [ H.button
                          [ HA.attribute "type" "button"
                          , HA.class "btn btn-warning"
                          , HE.onClick (events.metaitemtodelete key)
                          ] [ H.text "delete" ]
                      ]
                ]
        addfields key val =
            H.div [ HA.class "form-row" ]
                [ H.div [ HA.class "col-3" ]
                      [ H.input
                            [ HA.attribute "type" "text"
                            , HA.class "form-control"
                            , HA.placeholder "key"
                            , HA.value key
                            , HE.onInput events.newkey
                            ] [ ]
                      ]
                , H.div [ HA.class "col-6" ]
                    [ H.input [ HA.attribute "type" "text"
                              , HA.class "form-control"
                              , HA.placeholder "value"
                              , HA.value <| val
                              , HE.onInput events.newvalue
                              ] [ ]
                    ]
                ]
        editfields ab = deletefields (U.first ab) (U.snd ab)
    in
    H.div [ ]
        [ viewusermetaheader model events showtitle
        , H.form
              [ HE.onSubmit events.savemeta ]
              <| (List.map editfields (Dict.toList model.editeditems)) ++
                  [ H.button
                        [ HA.attribute "type" "submit"
                        , HA.class "btn btn-primary col-sm-10"
                        ]
                        [ H.text "save entries"]
                  ]
        , H.form [ HE.onSubmit events.addmetaitem ]
            [ addfields (U.first model.metaitem) (U.snd model.metaitem)
            , H.button
                  [ HA.attribute "type" "submit"
                  , HA.class "btn btn-primary col-sm-10"
                  ]
                  [ H.text "add entry"]
            ]
        ]

-- formula

getstring fromatom =
    case fromatom of
        Lisp.Expression _ ->
            "nope"
        Lisp.Atom atom ->
            case atom of
                Lisp.String str ->
                    str
                _ ->
                    "nope"


linkname model datatype arg =
    let
        name =
            getstring arg

        nameurl = case datatype of
            SeriesType _ -> UB.crossOrigin model.baseurl
                            [ "tsinfo" ] [ UB.string "name" name ]
            GroupType _ -> UB.crossOrigin model.baseurl
                            [ "groupinfo" ] [ UB.string "name" name ]

    in
    [ H.a [ HA.class "s"
          , HA.href nameurl
          ]
          [ H.span
                [ HA.class "s" ]
                [ H.text <| Lisp.quote ++ name ++ Lisp.quote ]
          ]
    ]


viewname model datatype index arg baseview =
    -- decorate the name in (series "<name>" ...) or (group "<name>" ...)
    case index of
        0 -> linkname model datatype arg
        _ -> baseview arg


viewintegrationnames model datatype index arg baseview =
    -- decorate the names in (integration "<name1>" "<name2>" ...)
    case index of
        0 -> linkname model datatype arg
        1 -> linkname model datatype arg
        _ -> baseview arg


viewformula model toggleevent =
    let
        depthslider formula =
            case model.formula_maxdepth of
                0 -> [ ]
                maxdepth ->
                    [ H.div
                          [ HA.title <|
                                "expand the formula to the desired depth (max " ++
                                (String.fromInt (maxdepth + 1)) ++ ")"
                          ]
                          [ H.input
                                [ HA.attribute "type" "range"
                                , HA.min "0"
                                , HA.max <| String.fromInt maxdepth
                                , HA.value <| String.fromInt model.formula_depth
                                , HA.step "1"
                                , HA.style "width"
                                    <| (String.fromInt <| model.formula_maxdepth * 5) ++ "em"
                                , HA.id "expand-formula"
                                , HE.onInput toggleevent
                                ] [ ]
                          ]
                    ]
        displayformula =
            -- fall back to the level 0 formula if there are still
            -- in flight queries
            case Dict.get model.formula_depth model.formula of
                Just formula ->
                    Just formula

                Nothing ->
                    case Dict.get 0 model.formula of
                        Just formula -> Just formula
                        Nothing -> Nothing
    in
    case displayformula of
        Nothing -> H.div [ ] [ ]
        Just formula ->
            H.div [ ]
                <| [ H.h2 [ ] [ H.text "Formula" ] ]
                    ++ (depthslider formula)
                    ++ [ showformula model formula ]


showformula model formula =
    let
        viewparsed parsed =
            case parsed of
                Nothing -> []
                Just parsedformula ->
                    Lisp.view parsedformula <|
                        Dict.fromList
                            [ ( "series", viewname model (SeriesType Primary))
                            , ( "group", viewname model (GroupType GroupPrimary))
                            , ( "integration", viewintegrationnames model (SeriesType Primary))
                            ]
    in H.span [] <| viewparsed <| Lisp.parse formula


viewoldformulas model =
    let
        renderitem item =
            H.tr [ ]
                [ H.td [ ] [ H.text <| cleandate item.stamp ]
                , H.td [ ] [ showformula model item.formula ]
                , H.td [ ] [ H.text item.user ]
                ]

        renderall =
            H.table [ HA.class "table w-auto" ]
                [ H.thead [ ]
                      [ H.tr [ ]
                            [ H.td [ ] [ H.text "Archived at" ]
                            , H.td [ ] [ H.text "Formula" ]
                            , H.td [ ] [ H.text "User" ]
                            ]
                      ]
                , H.tbody [ ]
                    <| List.map renderitem model.oldformulas
                ]

    in renderall


metadicttostring d =
    let
        builditem ab =
            U.first ab ++ " → " ++ (M.metavaltostring <| U.snd ab)
    in
    String.join "," <| List.map builditem (Dict.toList d)


viewlogentry entry =
    H.tr [ ]
        [ H.th [ HA.scope "row" ]
              [ H.text (String.fromInt entry.rev) ]
        , H.td [ ] [ H.text entry.author ]
        , H.td [ ] [ H.text ( cleandate entry.date )]
        , H.td [ ] [ H.text <| metadicttostring entry.meta ]
        ]


viewDatesRange insertionDates dateIndex debouncerMsg dateMsg =
    let
        numidates = Array.length insertionDates
        currdate =
            case Array.get dateIndex insertionDates of
                Nothing -> ""
                Just date -> date
    in
    if numidates < 2
    then
        H.div []
            [ H.input
                  [ HA.attribute "type" "range"
                  , HA.class "form-control-range"
                  , HA.disabled True ]
                  []
            ]
    else
        H.map (provideInput >> debouncerMsg) <|
            H.div []
            [ H.input
                  [ HA.attribute "type" "range"
                  , HA.min "0"
                  , HA.max (String.fromInt (numidates - 1))
                  , HA.value (String.fromInt dateIndex)
                  , HA.class "form-control-range"
                  , HA.title currdate
                  , HE.onInput dateMsg
                  ] [ ]
            ]


viewWidgetIdates: Bool -> Array String -> Int -> (Bool -> Direction -> msg) -> H.Html msg
viewWidgetIdates history idates index msg =
    let
        idate =
            Maybe.withDefault ""
                ( Array.get
                      index
                      idates
                )
        ( previous, pactive ) =
            maybeDate idates ( index - 1 )
        ( next, nactive ) =
            maybeDate idates ( index + 1 )
    in
    H.div
        [ HA.class "widget-idates" ]
        [ H.div
            ([ HA.class "idate-adjacent"
             , HA.title "previous date"
             ] ++ ( if pactive
                    then [ HA.class "idate-exists"
                         , HE.onClick ( msg history Prev )
                         ]
                    else []
                  )
            )
            [ H.text (formatIDate previous Left pactive)]
        , H.div
            [ HA.class "idate-history"
            , HA.title "current revdate"
            ]
            [ H.text ( formatIDate idate Center True)]
        , H.div
            ([ HA.class "idate-adjacent button"
             , HA.title "next date"
             ] ++ ( if nactive
                    then [ HA.class "idate-exists"
                         , HE.onClick ( msg history Next ) ]
                    else []
                  )
            )
            [ H.text ( formatIDate next Right nactive ) ]
        ]


maybeDate: Array String -> Int -> ( String, Bool )
maybeDate idates idx =
    case (Array.get idx idates) of
        Just date -> ( date, True)
        Nothing -> ( "", False )


formatIDate: String -> Position -> Bool -> String
formatIDate date position actif =
    if not actif
    then ""
    else
        let fdate =
                String.replace "T" " "
                    ( String.left 16 date)
        in case position of
               Center -> fdate
               Left -> "<< " ++ fdate
               Right -> fdate ++ " >>"


viewgraph data layoutOptions options inferredFreq =
    let
        plot (name, plotdata) =
            scatterplot
                name
                (Dict.keys plotdata)
                (Dict.values plotdata)
                (if inferredFreq then "lines+markers" else "lines")
                options
        plots = List.map plot <| Dict.toList data
        args =
            serializedPlotArgs
                "plot"
                plots
                layoutOptions
                defaultConfigOptions
    in
    H.div
        [ ]
        [ H.div [ HA.id "plot" ] [ ]
        , H.node "plot-figure" [ HA.attribute "args" args ] [ ]
        ]


viewHistoryGraph model =
    let
        currentIdate = Maybe.withDefault
            "" (Array.get model.historyDateIndex model.lastIdates)
        lastIdate = Maybe.withDefault
            ""
            (List.last (Array.toList model.lastIdates))
        partialOption = buildOptions
                            defaultTraceOptions
                            lastIdate
                            currentIdate
        listTraces = List.map
                        ( buildTrace partialOption)
                        (Dict.toList model.historyPlots)
        historyArgs = serializedPlotArgs
                        "plot-history"
                        listTraces
                        defaultLayoutOptions
                        defaultConfigOptions
    in
    H.div
        [ ]
        [ H.div [ HA.id "plot-history" ] [ ]
        , H.node "plot-figure" [ HA.attribute "history-args" historyArgs ] [ ]
        ]


buildOptions: TraceOptions -> String -> String -> String -> TraceOptions
buildOptions default lastIdate currentIdate idate =
    { default | line = Just
                          { color =
                                if idate == lastIdate
                                then "rgb(0, 0, 0)"
                                else if idate < currentIdate
                                     then "rgb(20, 200, 20)"
                                     else if idate == currentIdate
                                          then "rgb(0, 0, 250)"
                                          else "rgb(204, 12, 20)"
                             }
              , showlegend = if (idate == lastIdate) || (idate == currentIdate)
                                then True
                                else False
              , opacity = if (idate == lastIdate) || (idate == currentIdate)
                            then 1
                            else 0.2
    }


buildTrace:  (String -> TraceOptions) -> ( String , Dict String ( Maybe Float )) -> Trace
buildTrace partialOption ( idate, series )  =
    { type_ = "scatter"
    , name = cleandate idate
    , x = Dict.keys series
    , y = Dict.values series
    , mode = "lines"
    , options = partialOption idate
    }


viewHoverGraph dictData =
    let
        title =
            "Application date : " ++ dictData.name
        sortedData =
            List.sortBy .date dictData.data
        dates =
            List.map (\dict -> dict.date) sortedData
        values =
            List.map (\dict -> Just dict.value) sortedData
        plot =
            scatterplot
                dictData.name
                dates
                values
                "lines"
                defaultTraceOptions
        hoverArgs =
            serializedPlotArgs
                "plot-hover"
                [ plot ]
                { defaultLayoutOptions
                    | title = Just title
                    , margin = { t = 45
                               , b = 110
                               , l = 40
                               , r = 20
                               }
                }
                defaultConfigOptions
    in
    H.div
        [ ]
        [ H.div [ HA.id "plot-hover" ] [ ]
        , H.node "plot-figure" [ HA.attribute "hover-args" hoverArgs ] [ ]
        ]


viewlog model showtitle msgLogNumber msgSeeLogs =
    if List.length model.log > 0 then
        H.div
            [ ]
            [ if showtitle
              then H.h2 [ ] [ H.text "History Log" ]
              else H.span [ ] [ ]
            , loglimitinput model msgLogNumber msgSeeLogs
            , H.table
                [ HA.class "table table-striped table-hover table-sm" ]
                [ H.thead [ ]
                      [ H.td [ HA.scope "col" ] [ H.text "#" ]
                      , H.td [ HA.scope "col" ] [ H.text "author" ]
                      , H.td [ HA.scope "col" ] [ H.text "date" ]
                      , H.td [ HA.scope "col" ] [ H.text "meta" ]
                      ]
                , H.tbody [ ] <| List.map viewlogentry (List.reverse model.log)
                ]
            ]
    else H.div [ ] [ ]


loglimitinput model msgLogNumber msgSeeLogs =
    let
        logNumber = case model.logsNumber of
            Just number ->
                String.fromInt number
            Nothing ->
                ""
    in
    H.div
        [ ]
        [ H.text "Logs displayed : "
        , H.input
            [ HA.class "form-control-sm"
            , HA.placeholder "Enter a valid number"
            , HA.attribute "type" "text"
            , HA.value logNumber
            , HE.onInput msgLogNumber
            ]
            []
        , H.span [] [ H.text " " ]
        , H.button
            [ HA.attribute "type" "button"
            , HA.class "btn btn-primary btn-sm"
            , HE.onClick msgSeeLogs
            ]
            [ H.text "See more logs" ]
        ]


viewdeletion model events  =
    if model.source /= "local" then H.span [] [] else
    if model.deleting then
        H.div [ ]
            [ H.button
                  [ HA.type_ "button"
                  , HA.class "btn btn-warning"
                  , HE.onClick events.confirmdeletion
                  ]
                  [ H.text "confirm" ]
            , H.button
                  [ HA.type_ "button"
                  , HA.class "btn btn-success"
                  , HE.onClick events.canceldeletion
                  ]
                  [ H.text "cancel" ]
            ]
    else
        H.div [ ]
            [ H.button
                  [ HA.type_ "button"
                  , HA.class "btn btn-danger"
                  , HE.onClick events.askdeletion ]
                  [ H.text "delete" ]
            ]


viewrenameaction model events =
    if model.source /= "local" then H.span [] [] else
    if model.renaming then
        let
            value =
                case model.newname of
                    Nothing -> model.name
                    Just newname -> newname
        in
        H.div [ ]
            [ H.input [ HA.class "form-control-sm"
                      , HA.size 80
                      , HA.type_ "text"
                      , HA.placeholder "new name"
                      , HA.value value
                      , HE.onInput events.editnewname
                      ] [ ]
            , case model.newname of
                  Just newname ->
                      H.button
                          [ HA.type_ "button"
                          , HA.class "btn btn-warning"
                          , HE.onClick events.confirmrename
                          ]
                          [ H.text "confirm" ]
                  Nothing -> H.span [ ] [ ]
            , H.button
                  [ HA.type_ "button"
                  , HA.class "btn btn-success"
                  , HE.onClick events.cancelrename
                  ]
                  [ H.text "cancel" ]
            ]
    else
        H.div [ HA.style "float" "right" ]
            [ H.button
                  [ HA.type_ "button"
                  , HA.class "btn btn-primary"
                  , HE.onClick events.askrename ]
                  [ H.text "rename" ]
            ]


msgdoesnotexist dtype =
    H.div
        []
        [ H.text (dtype ++ " does not exists. Check your url.") ]
