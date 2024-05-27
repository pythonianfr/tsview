module HtmlData.Attributes exposing (Attribute(..), EventListener(..), accept, acceptCharset, accesskey, action, align, alt, property, attribute, autocomplete, autofocus, autoplay, checked, cite, class, classList, cols, colspan, contenteditable, contextmenu, controls, coords, datetime, default, dir, disabled, download, draggable, dropzone, enctype, for, form, headers, height, hidden, href, hreflang, id, ismap, itemprop, kind, lang, list, loop, manifest, map, max, maxlength, media, method, min, minlength, multiple, name, novalidate, pattern, ping, placeholder, poster, preload, pubdate, readonly, rel, required, reversed, rows, rowspan, sandbox, scope, selected, shape, size, spellcheck, src, srcdoc, srclang, start, step, style, tabindex, target, title, type_, usemap, value, width, wrap)

{-| See documentation of [Html.Attributes](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes)

Except, the types here are intentionally fully exposed.
The helper functions exists merely to make writing them feel like elm/html et al

@docs Attribute, EventListener, accept, acceptCharset, accesskey, action, align, alt, property, attribute, autocomplete, autofocus, autoplay, checked, cite, class, classList, cols, colspan, contenteditable, contextmenu, controls, coords, datetime, default, dir, disabled, download, draggable, dropzone, enctype, for, form, headers, height, hidden, href, hreflang, id, ismap, itemprop, kind, lang, list, loop, manifest, map, max, maxlength, media, method, min, minlength, multiple, name, novalidate, pattern, ping, placeholder, poster, preload, pubdate, readonly, rel, required, reversed, rows, rowspan, sandbox, scope, selected, shape, size, spellcheck, src, srcdoc, srclang, start, step, style, tabindex, target, title, type_, usemap, value, width, wrap

-}

import Json.Decode


{-| -}
type Attribute msg
    = Attribute String String
    | NoAttribute
    | EventListener (EventListener msg)
    | Property String Json.Decode.Value


{-| Stores declarations for [Html.Events](https://package.elm-lang.org/packages/elm/html/latest/Html-Events)
-}
type EventListener msg
    = On String (Json.Decode.Decoder msg)
    | OnInput (String -> msg)
    | OnCheck (Bool -> msg)
    | StopPropagationOn String (Json.Decode.Decoder ( msg, Bool ))
    | PreventDefaultOn String (Json.Decode.Decoder ( msg, Bool ))
    | Custom String (Json.Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool })


{-| -}
style : String -> String -> Attribute msg
style k v =
    Attribute "style" (k ++ ":" ++ v)


{-| -}
attribute : String -> String -> Attribute msg
attribute =
    Attribute


{-| -}
property : String -> Json.Decode.Value -> Attribute msg
property =
    Property


{-| -}
classList : List ( String, Bool ) -> Attribute msg
classList tuples =
    tuples
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> String.join " "
        |> Attribute "class"


{-| -}
class : String -> Attribute msg
class =
    Attribute "class"


{-| -}
map : (a -> msg) -> Attribute a -> Attribute msg
map f attr =
    case attr of
        Attribute k v ->
            Attribute k v

        NoAttribute ->
            NoAttribute

        EventListener l ->
            EventListener (mapEventListener f l)

        Property k v ->
            Property k v


mapEventListener : (a -> b) -> EventListener a -> EventListener b
mapEventListener f listener =
    case listener of
        On s d ->
            On s (Json.Decode.map f d)

        OnInput msg ->
            OnInput (msg >> f)

        OnCheck msg ->
            OnCheck (msg >> f)

        StopPropagationOn s d ->
            StopPropagationOn s (Json.Decode.map (Tuple.mapFirst f) d)

        PreventDefaultOn s d ->
            PreventDefaultOn s (Json.Decode.map (Tuple.mapFirst f) d)

        Custom s d ->
            Custom s
                (Json.Decode.map
                    (\a ->
                        { message = f a.message
                        , stopPropagation = a.stopPropagation
                        , preventDefault = a.preventDefault
                        }
                    )
                    d
                )


{-| -}
id : String -> Attribute msg
id =
    Attribute "id"


{-| -}
title : String -> Attribute msg
title =
    Attribute "title"


{-| -}
hidden : Bool -> Attribute msg
hidden bool =
    if bool then
        Attribute "hidden" ""

    else
        NoAttribute


{-| -}
type_ : String -> Attribute msg
type_ =
    Attribute "type"


{-| -}
value : String -> Attribute msg
value =
    Attribute "value"


{-| -}
checked : Bool -> Attribute msg
checked bool =
    if bool then
        Attribute "checked" ""

    else
        NoAttribute


{-| -}
placeholder : String -> Attribute msg
placeholder =
    Attribute "placeholder"


{-| -}
selected : Bool -> Attribute msg
selected bool =
    if bool then
        Attribute "selected" ""

    else
        NoAttribute


{-| -}
accept : String -> Attribute msg
accept =
    Attribute "accept"


{-| -}
acceptCharset : String -> Attribute msg
acceptCharset =
    Attribute "acceptCharset"


{-| -}
action : String -> Attribute msg
action =
    Attribute "action"


{-| -}
autocomplete : Bool -> Attribute msg
autocomplete bool =
    if bool then
        Attribute "autocomplete" ""

    else
        NoAttribute


{-| -}
autofocus : Bool -> Attribute msg
autofocus bool =
    if bool then
        Attribute "autofocus" ""

    else
        NoAttribute


{-| -}
disabled : Bool -> Attribute msg
disabled bool =
    if bool then
        Attribute "disabled" ""

    else
        NoAttribute


{-| -}
enctype : String -> Attribute msg
enctype =
    Attribute "enctype"


{-| -}
list : String -> Attribute msg
list =
    Attribute "list"


{-| -}
maxlength : Int -> Attribute msg
maxlength i =
    Attribute "maxlength" (String.fromInt i)


{-| -}
minlength : Int -> Attribute msg
minlength i =
    Attribute "minlength" (String.fromInt i)


{-| -}
method : String -> Attribute msg
method =
    Attribute "method"


{-| -}
multiple : Bool -> Attribute msg
multiple bool =
    if bool then
        Attribute "multiple" ""

    else
        NoAttribute


{-| -}
name : String -> Attribute msg
name =
    Attribute "name"


{-| -}
novalidate : Bool -> Attribute msg
novalidate bool =
    if bool then
        Attribute "novalidate" ""

    else
        NoAttribute


{-| -}
pattern : String -> Attribute msg
pattern =
    Attribute "pattern"


{-| -}
readonly : Bool -> Attribute msg
readonly bool =
    if bool then
        Attribute "readonly" ""

    else
        NoAttribute


{-| -}
required : Bool -> Attribute msg
required bool =
    if bool then
        Attribute "required" ""

    else
        NoAttribute


{-| -}
size : Int -> Attribute msg
size i =
    Attribute "size" (String.fromInt i)


{-| -}
for : String -> Attribute msg
for =
    Attribute "for"


{-| -}
form : String -> Attribute msg
form =
    Attribute "form"


{-| -}
max : String -> Attribute msg
max =
    Attribute "max"


{-| -}
min : String -> Attribute msg
min =
    Attribute "min"


{-| -}
step : String -> Attribute msg
step =
    Attribute "step"


{-| -}
cols : Int -> Attribute msg
cols i =
    Attribute "cols" (String.fromInt i)


{-| -}
rows : Int -> Attribute msg
rows i =
    Attribute "rows" (String.fromInt i)


{-| -}
wrap : String -> Attribute msg
wrap =
    Attribute "wrap"


{-| -}
href : String -> Attribute msg
href =
    Attribute "href"


{-| -}
target : String -> Attribute msg
target =
    Attribute "target"


{-| -}
download : String -> Attribute msg
download =
    Attribute "download"


{-| -}
hreflang : String -> Attribute msg
hreflang =
    Attribute "hreflang"


{-| -}
media : String -> Attribute msg
media =
    Attribute "media"


{-| -}
ping : String -> Attribute msg
ping =
    Attribute "ping"


{-| -}
rel : String -> Attribute msg
rel =
    Attribute "rel"


{-| -}
ismap : Bool -> Attribute msg
ismap bool =
    if bool then
        Attribute "ismap" ""

    else
        NoAttribute


{-| -}
usemap : String -> Attribute msg
usemap =
    Attribute "usemap"


{-| -}
shape : String -> Attribute msg
shape =
    Attribute "shape"


{-| -}
coords : String -> Attribute msg
coords =
    Attribute "coords"


{-| -}
src : String -> Attribute msg
src =
    Attribute "src"


{-| -}
height : Int -> Attribute msg
height i =
    Attribute "height" (String.fromInt i)


{-| -}
width : Int -> Attribute msg
width i =
    Attribute "width" (String.fromInt i)


{-| -}
alt : String -> Attribute msg
alt =
    Attribute "alt"


{-| -}
autoplay : Bool -> Attribute msg
autoplay bool =
    if bool then
        Attribute "autoplay" ""

    else
        NoAttribute


{-| -}
controls : Bool -> Attribute msg
controls bool =
    if bool then
        Attribute "controls" ""

    else
        NoAttribute


{-| -}
loop : Bool -> Attribute msg
loop bool =
    if bool then
        Attribute "loop" ""

    else
        NoAttribute


{-| -}
preload : String -> Attribute msg
preload =
    Attribute "preload"


{-| -}
poster : String -> Attribute msg
poster =
    Attribute "poster"


{-| -}
default : Bool -> Attribute msg
default bool =
    if bool then
        Attribute "default" ""

    else
        NoAttribute


{-| -}
kind : String -> Attribute msg
kind =
    Attribute "kind"


{-| -}
srclang : String -> Attribute msg
srclang =
    Attribute "srclang"


{-| -}
sandbox : String -> Attribute msg
sandbox =
    Attribute "sandbox"


{-| -}
srcdoc : String -> Attribute msg
srcdoc =
    Attribute "srcdoc"


{-| -}
reversed : Bool -> Attribute msg
reversed bool =
    if bool then
        Attribute "reversed" ""

    else
        NoAttribute


{-| -}
start : Int -> Attribute msg
start i =
    Attribute "start" (String.fromInt i)


{-| -}
align : String -> Attribute msg
align =
    Attribute "align"


{-| -}
colspan : Int -> Attribute msg
colspan i =
    Attribute "colspan" (String.fromInt i)


{-| -}
rowspan : Int -> Attribute msg
rowspan i =
    Attribute "rowspan" (String.fromInt i)


{-| -}
headers : String -> Attribute msg
headers =
    Attribute "headers"


{-| -}
scope : String -> Attribute msg
scope =
    Attribute "scope"


{-| -}
accesskey : Char -> Attribute msg
accesskey c =
    Attribute "accesskey" (String.fromChar c)


{-| -}
contenteditable : Bool -> Attribute msg
contenteditable bool =
    if bool then
        Attribute "contenteditable" ""

    else
        NoAttribute


{-| -}
contextmenu : String -> Attribute msg
contextmenu =
    Attribute "contextmenu"


{-| -}
dir : String -> Attribute msg
dir =
    Attribute "dir"


{-| -}
draggable : String -> Attribute msg
draggable =
    Attribute "draggable"


{-| -}
dropzone : String -> Attribute msg
dropzone =
    Attribute "dropzone"


{-| -}
itemprop : String -> Attribute msg
itemprop =
    Attribute "itemprop"


{-| -}
lang : String -> Attribute msg
lang =
    Attribute "lang"


{-| -}
spellcheck : Bool -> Attribute msg
spellcheck bool =
    if bool then
        Attribute "spellcheck" ""

    else
        NoAttribute


{-| -}
tabindex : Int -> Attribute msg
tabindex i =
    Attribute "tabindex" (String.fromInt i)


{-| -}
cite : String -> Attribute msg
cite =
    Attribute "cite"


{-| -}
datetime : String -> Attribute msg
datetime =
    Attribute "datetime"


{-| -}
pubdate : String -> Attribute msg
pubdate =
    Attribute "pubdate"


{-| -}
manifest : String -> Attribute msg
manifest =
    Attribute "manifest"
