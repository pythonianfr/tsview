module Info exposing
    ( delete
    , formuladecoder
    , getformula
    , getidates
    , getwriteperms
    , idatesdecoder
    , metatype
    , rename
    , savemeta
    , SeriesType(..)
    , viewerrors
    , viewformula
    , viewdatespicker
    , viewdeletion
    , viewlog
    , viewmeta
    , viewrenameaction
    , viewseealso
    , viewusermeta
    )


import Array exposing (Array)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Json.Encode as E
import JsonTree as JT exposing (TaggedValue(..))
import Lisp
import Metadata as M
import Url.Builder as UB
import Util as U


type SeriesType
    = Primary
    | Formula


getwriteperms urlprefix event =
    Http.get
        { expect = Http.expectString event
        , url = UB.crossOrigin urlprefix [ "tsinfo", "canwrite" ] [ ]
        }


type alias FormulaResponse =
    { level : Int
    , formula : String
    }


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


getdepth model name callback =
    Http.get
        { expect = Http.expectString callback
        , url =
            UB.crossOrigin model.baseurl
                [ "api", "series", "formula_depth" ]
                [ UB.string "name" name
                , UB.int "display" 1
                , UB.int "level" <| model.formula_depth
                ]
        }


idatesdecoder : D.Decoder (List String)
idatesdecoder =
    D.field "insertion_dates" (D.list D.string)


getidates model dtype callback =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", dtype, "insertion_dates" ]
              [ UB.string "name" model.name
              , UB.int "nocache" <| U.bool2int model.view_nocache
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


viewerrors model =
    if List.length model.errors > 0 then
    H.div [ ]
        [ H.h2 [ ] [ H.text "Errors" ]
        , H.div [ ] <| List.map (\x -> H.p [ ] [ H.text x ]) model.errors
        ]
    else H.span [ ] [ ]


viewdatespicker model events =
    let
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> U.cleanupdate date

        idate =
            [ H.label [ HA.for "idate-picker" ] [ H.text "revision date" ]
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
            , H.span [ ] [ H.text " " ]
            , H.input [ HA.type_ "date"
                      , HA.id "fvd-picker"
                      , HA.name "fvd-picker"
                      , HA.value model.mindate
                      , HE.onInput events.fvdatepickerchanged
                      ] [ ]
            ]

        tvdate =
            [ H.label [ HA.for "tvd-picker" ] [ H.text "to value date" ]
            , H.span [ ] [ H.text " " ]
            , H.input [ HA.type_ "date"
                      , HA.id "tvd-picker"
                      , HA.name "tvd-picker"
                      , HA.value model.maxdate
                      , HE.onInput events.tvdatepickerchanged
                      ] [ ]
            ]

        spacer = [ H.span [ ] [ H.text " " ] ]

    in H.div
        [ ]
        (idate ++ spacer ++ fvdate ++ spacer ++ tvdate)


viewmeta model =
    let
        hidden = [ "index_names", "index_type", "index_dtype", "value_dtype" ]
        fixval name val =
            if name == "supervision_status" && val == ""
            then "formula"
            else val
        elt name =
            H.li [ ] [ H.text <| name
                        ++ " → "
                        ++ (fixval name <| M.dget name model.meta)
                        ++ " ["
                        ++ (metatype <| Dict.get name model.meta)
                        ++ "]"
                   ]
    in
    H.div [ ]
    [ H.h2 [ ] [ H.text "Metadata" ]
    , H.ul [ ] <| List.map elt <| List.filter (\x -> not <| List.member x hidden) M.metanames
    ]


viewusermetaheader model events =
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
        H.h2  [ ]
            [ H.text "User Metadata"
            , H.span [ ] [ H.text " "]
            , editaction
            ]


viewusermeta model events =
    if model.editing then editusermeta model events else
    let
        elt (k, v) =
            H.li [ ] [ H.text <| k
                           ++ " → "
                           ++ (M.metavaltostring v)
                           ++ " ["
                           ++ (metatype <| Just v)
                           ++ "]"
                   ]
    in
    if not <| Dict.isEmpty model.usermeta then
        H.div [ ]
            [ viewusermetaheader model events
            , H.ul [ ] <| List.map elt (Dict.toList model.usermeta)
            ]
    else
        H.div [ ]
            [ viewusermetaheader model events
            , H.text "No user-defined metadata yet."
            ]


editusermeta model events =
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
        [ viewusermetaheader model events
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
        Lisp.Expression _ -> "nope"
        Lisp.Atom atom ->
            case atom of
                Lisp.String str -> str
                _ -> "nope"


linkname model arg =
    let
        name =
            getstring arg

        nameurl =
            UB.crossOrigin model.baseurl
                [ "tsinfo" ] [ UB.string "name" name ]
    in
    [ H.a [ HA.class "s"
          , HA.href nameurl
          ]
          [ H.span
                [ HA.class "s" ]
                [ H.text <| Lisp.quote ++ name ++ Lisp.quote ]
          ]
    ]


viewseriesname model index arg baseview =
    -- decorate the name in (series "<name>" ...)
    case index of
        0 -> linkname model arg
        _ -> baseview arg


viewintegrationnames model index arg baseview =
    -- decorate the names in (integration "<name1>" "<name2>" ...)
    case index of
        0 -> linkname model arg
        1 -> linkname model arg
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

        viewparsed parsed =
            case parsed of
                Nothing -> []
                Just parsedformula ->
                    Lisp.view parsedformula <|
                        Dict.fromList [ ("series", viewseriesname model)
                                      , ("integration", viewintegrationnames model)
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
                    ++ [ H.span [] <| viewparsed <| Lisp.parse formula ]


viewseealso model =
    let
        editorlabel =
            case model.seriestype of
                Primary ->  "edit values"
                Formula ->  "show values"
    in
    H.div [ ]
        [ H.div [ ]
              [ H.span [ ] [ H.text " ⇒ " ]
              , H.a [ HA.href <| UB.crossOrigin
                          model.baseurl
                          [ "tshistory", model.name ] [ ]
                    , HA.target "_blank"
                    ] [ H.text "browse history" ]
              ]
        , H.div [ ]
            [ H.span [ ] [ H.text " ⇒ " ]
            , H.a [ HA.href <| UB.crossOrigin
                        model.baseurl
                        [ "tseditor" ]
                        [ UB.string "name" model.name ]
                  , HA.target "_blank"
                  ] [ H.text editorlabel ]
            ]
        , case model.seriestype of
              Formula ->
                  H.div [ ]
                      [ H.span [ ] [ H.text " ⇒ " ]
                      , H.a [ HA.href <| UB.crossOrigin
                                  model.baseurl
                                  [ "tsformula" ]
                                  [ UB.string "name" model.name ]
                            , HA.target "_blank"
                            ] [ H.text "edit formula" ]
                      ]

              Primary ->
                  H.span [ ] [ ]
        ]


metadicttostring d =
    let
        builditem ab =
            U.first ab ++ " → " ++ (M.metavaltostring <| U.snd ab)
    in
    String.join "," <| List.map builditem (Dict.toList d)


viewlogentry entry =
    H.tr [ ]
        [ H.th [ HA.scope "row" ] [ H.text (String.fromInt entry.rev) ]
        , H.td [ ] [ H.text entry.author ]
        , H.td [ ] [ H.text entry.date ]
        , H.td [ ] [ H.text <| metadicttostring entry.meta ]
        ]


viewlog model showtitle =
    if List.length model.log > 0 then
        H.div
            [ ]
            [ if showtitle
              then H.h2 [ ] [ H.text "History Log" ]
              else H.span [ ] [ ]
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


viewdeletion model dtype events =
    if model.deleting then
        H.div [ HA.style "float" "right" ]
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
        H.div [ HA.style "float" "right" ]
            [ H.button
                  [ HA.type_ "button"
                  , HA.class "btn btn-danger"
                  , HE.onClick events.askdeletion ]
                  [ H.text "delete" ]
            ]


viewrenameaction model dtype events =
    if model.renaming then
        let
            value =
                case model.newname of
                    Nothing -> model.name
                    Just newname -> newname
        in
        H.div [ HA.style "float" "right" ]
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
