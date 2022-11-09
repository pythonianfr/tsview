module Info exposing
    ( getwriteperms
    , idatesdecoder
    , metatype
    , viewerrors
    , viewusermetaheader
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


viewusermetaheader model editevent cancelevent =
    let
        editaction =
            if model.canwrite then
                if not model.editing then
                    H.button
                    [ HA.attribute "type" "button"
                    , HA.class "btn btn-primary"
                    , HE.onClick editevent
                    ] [ H.text "edit" ]
                else
                    H.button
                    [ HA.attribute "type" "button"
                    , HA.class "btn btn-warning"
                    , HE.onClick cancelevent
                    ] [ H.text "cancel" ]
            else H.span [ ] [ ]
    in
        H.h2  [ ]
            [ H.text "User Metadata"
            , H.span [ ] [ H.text " "]
            , editaction
            ]
