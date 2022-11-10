module Info exposing
    ( getformula
    , getidates
    , getcomponents
    , getwriteperms
    , idatesdecoder
    , metatype
    , savemeta
    , supervision
    , viewcomponents
    , viewerrors
    , viewformula
    , viewidatepicker
    , viewmeta
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
import Metadata as M
import Url.Builder as UB
import Util as U


getwriteperms urlprefix event =
    Http.get
        { expect = Http.expectString event
        , url = UB.crossOrigin urlprefix [ "tsinfo", "canwrite" ] [ ]
        }


getformula model dtype callback  =
    Http.get
        { expect = Http.expectString callback
        , url =
            UB.crossOrigin model.baseurl
                [ "api", dtype, "formula" ]
                [ UB.string "name" model.name
                , UB.int "display" 1
                , UB.int "expanded" <| U.bool2int model.formula_expanded
                ]
        }


getcomponents model dtype callback =
    Http.get
        { url =
              UB.crossOrigin
              model.baseurl
              [ "api", dtype, "formula_components" ]
              [ UB.string "name" model.name
              , UB.int "expanded" <| U.bool2int model.formula_expanded
              ]
        , expect = Http.expectString callback
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


viewidatepicker model events =
    let
        currdate =
            case Array.get model.date_index model.insertion_dates of
                Nothing -> ""
                Just date -> U.cleanupdate date
    in H.div
        [ ]
        [ H.label [ HA.for "idate-picker" ] [ H.text "revision date" ]
        , H.span [ ] [ H.text " " ]
        , H.input [ HA.type_ "datetime-local"
                  , HA.id "idate-picker"
                  , HA.name "idate-picker"
                  , HA.value currdate
                  , HE.onInput events.idatepickerchanged
                  ] [ ]
        , H.span [ ] [ H.text " " ]
        , H.label [ HA.for "fvd-picker" ] [ H.text "from value date" ]
        , H.span [ ] [ H.text " " ]
        , H.input [ HA.type_ "date"
                  , HA.id "fvd-picker"
                  , HA.name "fvd-picker"
                  , HA.value model.mindate
                  , HE.onInput events.fvdatepickerchanged
                  ] [ ]
        , H.span [ ] [ H.text " " ]
        , H.label [ HA.for "tvd-picker" ] [ H.text "to value date" ]
        , H.span [ ] [ H.text " " ]
        , H.input [ HA.type_ "date"
                  , HA.id "tvd-picker"
                  , HA.name "tvd-picker"
                  , HA.value model.maxdate
                  , HE.onInput events.tvdatepickerchanged
                  ] [ ]
        ]


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


viewformula model toggleevent =
    let
        maybeformula =
            case model.formula_expanded of
                True ->
                    case model.expanded_formula of
                        Nothing ->
                            -- let's keep showinng the old one
                            -- till the new has landed
                            model.formula
                        Just formula -> model.expanded_formula
                False -> model.formula
    in
    case maybeformula of
        Nothing -> H.div [ ] [ ]
        Just formula ->
            H.div [ ]
                [ H.h2 [ ] [ H.text "Formula" ]
                , H.div [ HA.class "custom-control custom-switch"
                      , HA.title <| if model.formula_expanded
                                   then "unexpand the formula"
                                   else "expand the formula"
                      ]
                     [ H.input
                           [ HA.attribute "type" "checkbox"
                           , HA.class "custom-control-input"
                           , HA.id "expand-formula"
                           , HE.onClick toggleevent
                           ] [ ]
                     , H.label
                         [ HA.class "custom-control-label"
                         , HA.for "expand-formula"
                         ]
                         [ H.text <| if model.formula_expanded
                                     then "expanded"
                                     else "unexpanded"
                         ]
                     ]
                , H.span [ ] <| U.tovirtualdom formula "could not parse the formula"
                ]


supervision model =
    case Dict.get "supervision_status" model.meta of
        Nothing -> "formula"
        Just x -> M.metavaltostring x


viewseealso model =
    let
        editorlabel =
            if (supervision model) /= "formula" then "edit values" else "show values"
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
        , if (supervision model) == "formula" then
              H.div [ ]
                  [ H.span [ ] [ H.text " ⇒ " ]
                  , H.a [ HA.href <| UB.crossOrigin
                              model.baseurl
                              [ "tsformula" ]
                              [ UB.string "name" model.name ]
                        , HA.target "_blank"
                      ] [ H.text "edit formula" ]
                  ]
          else H.span [ ] [ ]
        ]


viewcomponents model =
    let
        alink seriesname =
            H.a [ HA.href <| UB.crossOrigin model.baseurl
                      [ "tsinfo" ]
                      [ UB.string "name" seriesname ]
                ]
                [ H.text seriesname ]

        tuple2node tuple =
            H.li [ ] [ alink (Tuple.first tuple)
                     , H.span [ ] [ H.text " → " ]
                     , node2html <| Tuple.second tuple
                     ]

        node2html node =
            case node.value of
                JT.TString str -> H.li [ ] [ alink str ]
                JT.TFloat num -> H.li [ ] [ H.text <|  String.fromFloat num ]
                JT.TBool bool -> H.li [ ] [ H.text <| if bool then "True" else "False" ]
                JT.TList list -> H.ul [ HA.class "square" ] <| List.map node2html list
                JT.TDict dict ->
                    H.ul [ HA.class "square" ] <| (Dict.toList dict |> List.map tuple2node)
                JT.TNull -> H.span [ ] [ ]

        components comp =
            case comp of
                Nothing ->
                    H.span [ ] [ H.text "" ]
                Just node ->
                    node2html node
    in
    if supervision model == "formula" then
        H.div [ ]
            [ H.h2 [ ] [ H.text "Components" ]
            , components <| case model.formula_expanded of
                                True -> model.expanded_formula_components
                                False -> model.formula_components
            ]
    else H.div [ ] [ ]
