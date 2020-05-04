module TsView.AceEditor exposing
    ( Config
    , Msg(..)
    , default
    , edit
    , readOnly
    )

import Bool.Extra as Bool
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as Events
import Json.Decode as D
import Json.Encode as E


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
    [ A.attribute "cfg" jsonCfg
    , A.attribute "code" code
    ]


readOnly : Config -> String -> Html msg
readOnly cfg code =
    H.node
        "ace-readonly"
        (A.class "ace_readonly" :: encodeAttrs cfg code)
        []


onChange : (String -> Msg) -> H.Attribute Msg
onChange toMsg =
    D.at [ "detail", "value" ] D.string |> D.map toMsg |> Events.on "onChange"


edit : Config -> String -> Bool -> Html Msg
edit cfg code reload =
    let
        reloadAttr =
            if reload then
                A.attribute "reload" code

            else
                A.attribute "reload" ""

        attrs =
            encodeAttrs cfg code
    in
    H.node
        "ace-editor"
        (A.class "ace_edit" :: onChange Edited :: reloadAttr :: attrs)
        []
