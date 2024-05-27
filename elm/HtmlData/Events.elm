module HtmlData.Events exposing (..)

import Html.Events
import HtmlData.Attributes exposing (..)
import Json.Decode


on : String -> Json.Decode.Decoder msg -> Attribute msg
on s d =
    EventListener (On s d)


onClick : msg -> Attribute msg
onClick msg =
    on "click" (Json.Decode.succeed msg)


onDoubleClick : msg -> Attribute msg
onDoubleClick msg =
    on "dblclick" (Json.Decode.succeed msg)


onMouseDown : msg -> Attribute msg
onMouseDown msg =
    on "mousedown" (Json.Decode.succeed msg)


onMouseUp : msg -> Attribute msg
onMouseUp msg =
    on "mouseup" (Json.Decode.succeed msg)


onMouseEnter : msg -> Attribute msg
onMouseEnter msg =
    on "mouseenter" (Json.Decode.succeed msg)


onMouseLeave : msg -> Attribute msg
onMouseLeave msg =
    on "mouseleave" (Json.Decode.succeed msg)


onMouseOver : msg -> Attribute msg
onMouseOver msg =
    on "mouseover" (Json.Decode.succeed msg)


onMouseOut : msg -> Attribute msg
onMouseOut msg =
    on "mouseout" (Json.Decode.succeed msg)


onInput : (String -> msg) -> Attribute msg
onInput =
    OnInput >> EventListener


onCheck : (Bool -> msg) -> Attribute msg
onCheck =
    OnCheck >> EventListener


onSubmit : msg -> Attribute msg
onSubmit msg =
    on "submit" (Json.Decode.succeed msg)


onBlur : msg -> Attribute msg
onBlur msg =
    on "blur" (Json.Decode.succeed msg)


onFocus : msg -> Attribute msg
onFocus msg =
    on "focus" (Json.Decode.succeed msg)


stopPropagationOn : String -> Json.Decode.Decoder ( msg, Bool ) -> Attribute msg
stopPropagationOn s d =
    EventListener (StopPropagationOn s d)


preventDefaultOn : String -> Json.Decode.Decoder ( msg, Bool ) -> Attribute msg
preventDefaultOn s d =
    EventListener (PreventDefaultOn s d)


custom : String -> Json.Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool } -> Attribute msg
custom s d =
    EventListener (Custom s d)


targetValue : Json.Decode.Decoder String
targetValue =
    Html.Events.targetValue


targetChecked : Json.Decode.Decoder Bool
targetChecked =
    Html.Events.targetChecked


keyCode : Json.Decode.Decoder Int
keyCode =
    Html.Events.keyCode
