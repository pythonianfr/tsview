module HtmlData exposing (Html(..), text, node, map, div, h1, h2, h3, h4, h5, h6, p, hr, pre, blockquote, span, a, code, em, strong, i, b, u, sub, sup, br, ol, ul, li, dl, dt, dd, img, iframe, canvas, math, form, input, textarea, button, select, option, section, nav, article, aside, header, footer, address, main_, figure, figcaption, table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th, fieldset, legend, label, datalist, optgroup, output, progress, meter, audio, video, source, track, embed, object, param, ins, del, small, cite, dfn, abbr, time, var, samp, kbd, s, q, mark, ruby, rt, rp, bdi, bdo, wbr, details, summary, menuitem, menu)

{-| See documentation of [Html](https://package.elm-lang.org/packages/elm/html/latest/Html)

Except, the types here are intentionally fully exposed.
The helper functions exists merely to make writing them feel like elm/html et al

@docs Html, text, node, map, div, h1, h2, h3, h4, h5, h6, p, hr, pre, blockquote, span, a, code, em, strong, i, b, u, sub, sup, br, ol, ul, li, dl, dt, dd, img, iframe, canvas, math, form, input, textarea, button, select, option, section, nav, article, aside, header, footer, address, main_, figure, figcaption, table, caption, colgroup, col, tbody, thead, tfoot, tr, td, th, fieldset, legend, label, datalist, optgroup, output, progress, meter, audio, video, source, track, embed, object, param, ins, del, small, cite, dfn, abbr, time, var, samp, kbd, s, q, mark, ruby, rt, rp, bdi, bdo, wbr, details, summary, menuitem, menu

-}

import Html
import HtmlData.Attributes exposing (Attribute, href)


{-| -}
type Html msg
    = Text String
    | Element String (List (Attribute msg)) (List (Html msg))
    | KeyedElement String (List (Attribute msg)) (List ( String, Html msg ))
    | LazyElement (() -> Html msg) (() -> Html.Html msg)


{-| -}
text : String -> Html msg
text =
    Text


{-| -}
node : String -> List (Attribute msg) -> List (Html msg) -> Html msg
node =
    Element


{-| -}
map : (a -> msg) -> Html a -> Html msg
map f htmlnode =
    case htmlnode of
        Text string ->
            Text string

        Element string attrs children ->
            Element string (List.map (HtmlData.Attributes.map f) attrs) (List.map (map f) children)

        KeyedElement string attrs children ->
            KeyedElement string (List.map (HtmlData.Attributes.map f) attrs) (List.map (Tuple.mapSecond (map f)) children)

        LazyElement lazyf lazyHtml ->
            LazyElement (\_ -> lazyf () |> map f) (\_ -> Html.map f (lazyHtml ()))


{-| -}
div : List (Attribute msg) -> List (Html msg) -> Html msg
div =
    Element "div"


{-| -}
h1 : List (Attribute msg) -> List (Html msg) -> Html msg
h1 =
    Element "h1"


{-| -}
h2 : List (Attribute msg) -> List (Html msg) -> Html msg
h2 =
    Element "h2"


{-| -}
h3 : List (Attribute msg) -> List (Html msg) -> Html msg
h3 =
    Element "h3"


{-| -}
h4 : List (Attribute msg) -> List (Html msg) -> Html msg
h4 =
    Element "h4"


{-| -}
h5 : List (Attribute msg) -> List (Html msg) -> Html msg
h5 =
    Element "h5"


{-| -}
h6 : List (Attribute msg) -> List (Html msg) -> Html msg
h6 =
    Element "h6"


{-| -}
p : List (Attribute msg) -> List (Html msg) -> Html msg
p =
    Element "p"


{-| -}
hr : List (Attribute msg) -> List (Html msg) -> Html msg
hr =
    Element "hr"


{-| -}
pre : List (Attribute msg) -> List (Html msg) -> Html msg
pre =
    Element "pre"


{-| -}
blockquote : List (Attribute msg) -> List (Html msg) -> Html msg
blockquote =
    Element "blockquote"


{-| -}
span : List (Attribute msg) -> List (Html msg) -> Html msg
span =
    Element "span"


{-| -}
a : List (Attribute msg) -> List (Html msg) -> Html msg
a =
    Element "a"


{-| -}
code : List (Attribute msg) -> List (Html msg) -> Html msg
code =
    Element "code"


{-| -}
em : List (Attribute msg) -> List (Html msg) -> Html msg
em =
    Element "em"


{-| -}
strong : List (Attribute msg) -> List (Html msg) -> Html msg
strong =
    Element "strong"


{-| -}
i : List (Attribute msg) -> List (Html msg) -> Html msg
i =
    Element "i"


{-| -}
b : List (Attribute msg) -> List (Html msg) -> Html msg
b =
    Element "b"


{-| -}
u : List (Attribute msg) -> List (Html msg) -> Html msg
u =
    Element "u"


{-| -}
sub : List (Attribute msg) -> List (Html msg) -> Html msg
sub =
    Element "sub"


{-| -}
sup : List (Attribute msg) -> List (Html msg) -> Html msg
sup =
    Element "sup"


{-| -}
br : List (Attribute msg) -> List (Html msg) -> Html msg
br =
    Element "br"


{-| -}
ol : List (Attribute msg) -> List (Html msg) -> Html msg
ol =
    Element "ol"


{-| -}
ul : List (Attribute msg) -> List (Html msg) -> Html msg
ul =
    Element "ul"


{-| -}
li : List (Attribute msg) -> List (Html msg) -> Html msg
li =
    Element "li"


{-| -}
dl : List (Attribute msg) -> List (Html msg) -> Html msg
dl =
    Element "dl"


{-| -}
dt : List (Attribute msg) -> List (Html msg) -> Html msg
dt =
    Element "dt"


{-| -}
dd : List (Attribute msg) -> List (Html msg) -> Html msg
dd =
    Element "dd"


{-| -}
img : List (Attribute msg) -> List (Html msg) -> Html msg
img =
    Element "img"


{-| -}
iframe : List (Attribute msg) -> List (Html msg) -> Html msg
iframe =
    Element "iframe"


{-| -}
canvas : List (Attribute msg) -> List (Html msg) -> Html msg
canvas =
    Element "canvas"


{-| -}
math : List (Attribute msg) -> List (Html msg) -> Html msg
math =
    Element "math"


{-| -}
form : List (Attribute msg) -> List (Html msg) -> Html msg
form =
    Element "form"


{-| -}
input : List (Attribute msg) -> List (Html msg) -> Html msg
input =
    Element "input"


{-| -}
textarea : List (Attribute msg) -> List (Html msg) -> Html msg
textarea =
    Element "textarea"


{-| -}
button : List (Attribute msg) -> List (Html msg) -> Html msg
button =
    Element "button"


{-| -}
select : List (Attribute msg) -> List (Html msg) -> Html msg
select =
    Element "select"


{-| -}
option : List (Attribute msg) -> List (Html msg) -> Html msg
option =
    Element "option"


{-| -}
section : List (Attribute msg) -> List (Html msg) -> Html msg
section =
    Element "section"


{-| -}
nav : List (Attribute msg) -> List (Html msg) -> Html msg
nav =
    Element "nav"


{-| -}
article : List (Attribute msg) -> List (Html msg) -> Html msg
article =
    Element "article"


{-| -}
aside : List (Attribute msg) -> List (Html msg) -> Html msg
aside =
    Element "aside"


{-| -}
header : List (Attribute msg) -> List (Html msg) -> Html msg
header =
    Element "header"


{-| -}
footer : List (Attribute msg) -> List (Html msg) -> Html msg
footer =
    Element "footer"


{-| -}
address : List (Attribute msg) -> List (Html msg) -> Html msg
address =
    Element "address"


{-| -}
main_ : List (Attribute msg) -> List (Html msg) -> Html msg
main_ =
    Element "main_"


{-| -}
figure : List (Attribute msg) -> List (Html msg) -> Html msg
figure =
    Element "figure"


{-| -}
figcaption : List (Attribute msg) -> List (Html msg) -> Html msg
figcaption =
    Element "figcaption"


{-| -}
table : List (Attribute msg) -> List (Html msg) -> Html msg
table =
    Element "table"


{-| -}
caption : List (Attribute msg) -> List (Html msg) -> Html msg
caption =
    Element "caption"


{-| -}
colgroup : List (Attribute msg) -> List (Html msg) -> Html msg
colgroup =
    Element "colgroup"


{-| -}
col : List (Attribute msg) -> List (Html msg) -> Html msg
col =
    Element "col"


{-| -}
tbody : List (Attribute msg) -> List (Html msg) -> Html msg
tbody =
    Element "tbody"


{-| -}
thead : List (Attribute msg) -> List (Html msg) -> Html msg
thead =
    Element "thead"


{-| -}
tfoot : List (Attribute msg) -> List (Html msg) -> Html msg
tfoot =
    Element "tfoot"


{-| -}
tr : List (Attribute msg) -> List (Html msg) -> Html msg
tr =
    Element "tr"


{-| -}
td : List (Attribute msg) -> List (Html msg) -> Html msg
td =
    Element "td"


{-| -}
th : List (Attribute msg) -> List (Html msg) -> Html msg
th =
    Element "th"


{-| -}
fieldset : List (Attribute msg) -> List (Html msg) -> Html msg
fieldset =
    Element "fieldset"


{-| -}
legend : List (Attribute msg) -> List (Html msg) -> Html msg
legend =
    Element "legend"


{-| -}
label : List (Attribute msg) -> List (Html msg) -> Html msg
label =
    Element "label"


{-| -}
datalist : List (Attribute msg) -> List (Html msg) -> Html msg
datalist =
    Element "datalist"


{-| -}
optgroup : List (Attribute msg) -> List (Html msg) -> Html msg
optgroup =
    Element "optgroup"


{-| -}
output : List (Attribute msg) -> List (Html msg) -> Html msg
output =
    Element "output"


{-| -}
progress : List (Attribute msg) -> List (Html msg) -> Html msg
progress =
    Element "progress"


{-| -}
meter : List (Attribute msg) -> List (Html msg) -> Html msg
meter =
    Element "meter"


{-| -}
audio : List (Attribute msg) -> List (Html msg) -> Html msg
audio =
    Element "audio"


{-| -}
video : List (Attribute msg) -> List (Html msg) -> Html msg
video =
    Element "video"


{-| -}
source : List (Attribute msg) -> List (Html msg) -> Html msg
source =
    Element "source"


{-| -}
track : List (Attribute msg) -> List (Html msg) -> Html msg
track =
    Element "track"


{-| -}
embed : List (Attribute msg) -> List (Html msg) -> Html msg
embed =
    Element "embed"


{-| -}
object : List (Attribute msg) -> List (Html msg) -> Html msg
object =
    Element "object"


{-| -}
param : List (Attribute msg) -> List (Html msg) -> Html msg
param =
    Element "param"


{-| -}
ins : List (Attribute msg) -> List (Html msg) -> Html msg
ins =
    Element "ins"


{-| -}
del : List (Attribute msg) -> List (Html msg) -> Html msg
del =
    Element "del"


{-| -}
small : List (Attribute msg) -> List (Html msg) -> Html msg
small =
    Element "small"


{-| -}
cite : List (Attribute msg) -> List (Html msg) -> Html msg
cite =
    Element "cite"


{-| -}
dfn : List (Attribute msg) -> List (Html msg) -> Html msg
dfn =
    Element "dfn"


{-| -}
abbr : List (Attribute msg) -> List (Html msg) -> Html msg
abbr =
    Element "abbr"


{-| -}
time : List (Attribute msg) -> List (Html msg) -> Html msg
time =
    Element "time"


{-| -}
var : List (Attribute msg) -> List (Html msg) -> Html msg
var =
    Element "var"


{-| -}
samp : List (Attribute msg) -> List (Html msg) -> Html msg
samp =
    Element "samp"


{-| -}
kbd : List (Attribute msg) -> List (Html msg) -> Html msg
kbd =
    Element "kbd"


{-| -}
s : List (Attribute msg) -> List (Html msg) -> Html msg
s =
    Element "s"


{-| -}
q : List (Attribute msg) -> List (Html msg) -> Html msg
q =
    Element "q"


{-| -}
mark : List (Attribute msg) -> List (Html msg) -> Html msg
mark =
    Element "mark"


{-| -}
ruby : List (Attribute msg) -> List (Html msg) -> Html msg
ruby =
    Element "ruby"


{-| -}
rt : List (Attribute msg) -> List (Html msg) -> Html msg
rt =
    Element "rt"


{-| -}
rp : List (Attribute msg) -> List (Html msg) -> Html msg
rp =
    Element "rp"


{-| -}
bdi : List (Attribute msg) -> List (Html msg) -> Html msg
bdi =
    Element "bdi"


{-| -}
bdo : List (Attribute msg) -> List (Html msg) -> Html msg
bdo =
    Element "bdo"


{-| -}
wbr : List (Attribute msg) -> List (Html msg) -> Html msg
wbr =
    Element "wbr"


{-| -}
details : List (Attribute msg) -> List (Html msg) -> Html msg
details =
    Element "details"


{-| -}
summary : List (Attribute msg) -> List (Html msg) -> Html msg
summary =
    Element "summary"


{-| -}
menuitem : List (Attribute msg) -> List (Html msg) -> Html msg
menuitem =
    Element "menuitem"


{-| -}
menu : List (Attribute msg) -> List (Html msg) -> Html msg
menu =
    Element "menu"
