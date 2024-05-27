module AceEditor exposing
    ( Config
    , AnnotationType(..)
    , Annotation
    , Annotations
    , Msg(..)
    , default
    , edit
    , edit_
    , readOnly
    , readOnly_
    )

import Bool.Extra as Bool
import Maybe.Extra as Maybe
import Json.Decode as D
import Json.Encode as E

import List.Nonempty as NE exposing (Nonempty)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as Events


type alias Config =
    { theme : String
    , mode : String
    , fontSize : Int
    }

type AnnotationType
    = Info
    | Warning
    | Error

type alias Annotation =
    { annotationType : AnnotationType
    , rowPos : Int
    , colPos : Int
    , message : String
    }

type alias Annotations = Nonempty Annotation

default : Config
default =
    Config
        "ace/theme/xcode"
        "ace/mode/lisp"
        12


type Msg
    = Edited String


renderAnnotationType : AnnotationType -> String
renderAnnotationType x = case x of
    Info -> "info"
    Warning -> "warning"
    Error -> "error"

encodeAnnotation : Annotation -> E.Value
encodeAnnotation {annotationType, rowPos, colPos, message} = E.object
    [ ( "type", E.string <| renderAnnotationType annotationType )
    , ( "row", E.int <| rowPos - 1 )
    , ( "column", E.int <| colPos - 1  )
    , ( "text", E.string message )
    ]

encodeAttrs : Config -> Maybe Annotations -> String -> List (H.Attribute msg)
encodeAttrs cfg annotationsM code =
    let
        jsonCfg = E.encode 0 <| E.object
            [ ( "theme", E.string cfg.theme )
            , ( "mode", E.string cfg.mode )
            , ( "fontSize", E.int cfg.fontSize )
            ]

        annotations = Maybe.unwrap
            E.null
            (NE.toList >> E.list encodeAnnotation)
            annotationsM

        jsonPayload = E.encode 0 <| E.object
            [ ( "code", E.string code )
            , ( "annotations", annotations)
            ]
    in
    [ HA.attribute "cfg" jsonCfg
    , HA.attribute "payload" jsonPayload
    ]


readOnly : Config -> Maybe Annotations -> String -> Html msg
readOnly cfg annotations code =
    H.node
        "ace-readonly"
        (HA.class "ace_readonly" :: encodeAttrs cfg annotations code)
        []

readOnly_ : Maybe Annotations -> String -> Html msg
readOnly_ = readOnly default

onChange : (String -> Msg) -> H.Attribute Msg
onChange toMsg =
    D.at [ "detail", "value" ] D.string |> D.map toMsg |> Events.on "onChange"


edit : Config -> Maybe Annotations -> String -> Bool -> Html Msg
edit cfg annotations code reload =
    let
        reloadAttr = HA.attribute "reload" <| if reload then code else ""
        attrs = encodeAttrs cfg annotations code
    in
    H.node
        "ace-editor"
        (HA.class "ace_edit" :: onChange Edited :: reloadAttr :: attrs)
        []

edit_ : Maybe Annotations -> String -> Bool -> Html Msg
edit_ = edit default
