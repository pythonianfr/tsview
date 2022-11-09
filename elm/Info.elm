module Info exposing
    ( getwriteperms
    , idatesdecoder
    , metatype
    , viewerrors
    , viewusermeta
    , viewmeta
    )

import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Metadata as M
import Url.Builder as UB
import Util as U


getwriteperms urlprefix event =
    Http.get
        { expect = Http.expectString event
        , url = UB.crossOrigin urlprefix [ "tsinfo", "canwrite" ] [ ]
        }


idatesdecoder : D.Decoder (List String)
idatesdecoder =
    D.field "insertion_dates" (D.list D.string)


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
