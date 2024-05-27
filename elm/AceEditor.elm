module AceEditor exposing
    ( Config
    , Msg(..)
    , default
    , edit
    , edit_
    , readOnly
    , readOnly_
    )

import Bool.Extra as Bool
import Json.Decode as D
import Json.Encode as E

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as Events


type alias Config =
    { theme : String
    , mode : String
    , fontSize : Int
    }


default : Config
default =
    Config
        "ace/theme/xcode"
        "ace/mode/lisp"
        12


type Msg
    = Edited String


encodeAttrs : Config -> String -> List (H.Attribute msg)
encodeAttrs cfg code =
    let
        jsonCfg =
            E.object
                [ ( "theme", E.string cfg.theme )
                , ( "mode", E.string cfg.mode )
                , ( "fontSize", E.int cfg.fontSize )
                ]
                |> E.encode 0
    in
    [ HA.attribute "cfg" jsonCfg
    , HA.attribute "code" code
    ]


readOnly : Config -> String -> Html msg
readOnly cfg code =
    H.node
        "ace-readonly"
        (HA.class "ace_readonly" :: encodeAttrs cfg code)
        []

readOnly_ : String -> Html msg
readOnly_ = readOnly default

onChange : (String -> Msg) -> H.Attribute Msg
onChange toMsg =
    D.at [ "detail", "value" ] D.string |> D.map toMsg |> Events.on "onChange"


edit : Config -> String -> Bool -> Html Msg
edit cfg code reload =
    let
        reloadAttr = HA.attribute "reload" <| if reload then code else ""
        attrs = encodeAttrs cfg code
    in
    H.node
        "ace-editor"
        (HA.class "ace_edit" :: onChange Edited :: reloadAttr :: attrs)
        []

edit_ : String -> Bool -> Html Msg
edit_ = edit default
