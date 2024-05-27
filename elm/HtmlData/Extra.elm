module HtmlData.Extra exposing
    ( toTextHtml, toTextPlain, toElmHtml
    , fromHtmlParserNodes
    , SanitizeConfig, defaultSanitizeConfig, TextPlainConfig, defaultTextPlainConfig
    , escapeHtml, sanitize
    )

{-|


## Functions to

to convert `HtmlData.Html` values into `String`

@docs toTextHtml, toTextPlain, toElmHtml


## Functions from

@docs fromHtmlParserNodes


## Configs

default configurations on how content is sanitized for toTextHtml, and how layout is done for toTextPlain

@docs SanitizeConfig, defaultSanitizeConfig, TextPlainConfig, defaultTextPlainConfig

@docs escapeHtml, sanitize


## More ~tests~ examples

    import HtmlData exposing (..)
    import HtmlData.Attributes exposing (..)

    div []
        [ h1 [] [ text "Block-level elements" ]
        , p []
            [ text "In this article, we'll examine HTML block-level elements and how they differ from "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements" ] [ text "inline-level elements" ]
            , text "."
            ]
        , p []
            [ text "HTML ("
            , b [] [ text "Hypertext Markup Language" ]
            , text ") elements ... by CSS in the "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout" ] [ text "Flow Layout" ]
            , text ". A Block-level element occupies ... contents, thereby creating a \"block\"."
            , aside []
                [ strong [] [ text "Note:" ]
                , text " A block-level element always starts on a new line and ... as it can)."
                ]
            , h3 [] [ text "See also" ]
            , ol []
                [ li [] [ a [ href "" ] [ text "Inline elements" ] ]
                , li [] [ a [ href "" ] [ text "display" ] ]
                , li [] [ a [ href "" ] [ text "Block and Inline Layout in Normal Flow" ] ]
                ]
            ]
        ]
        |> toTextHtml
    --> "<div><h1>Block-level&#32;elements</h1><p>In&#32;this&#32;article,&#32;we&#39;ll&#32;examine&#32;HTML&#32;block-level&#32;elements&#32;and&#32;how&#32;they&#32;differ&#32;from&#32;<a href=\"https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements\">inline-level&#32;elements</a>.</p><p>HTML&#32;&#40;<b>Hypertext&#32;Markup&#32;Language</b>&#41;&#32;elements&#32;...&#32;by&#32;CSS&#32;in&#32;the&#32;<a href=\"https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout\">Flow&#32;Layout</a>.&#32;A&#32;Block-level&#32;element&#32;occupies&#32;...&#32;contents,&#32;thereby&#32;creating&#32;a&#32;&quot;block&quot;.<aside><strong>Note:</strong>&#32;A&#32;block-level&#32;element&#32;always&#32;starts&#32;on&#32;a&#32;new&#32;line&#32;and&#32;...&#32;as&#32;it&#32;can&#41;.</aside><h3>See&#32;also</h3><ol><li><a href=\"\">Inline&#32;elements</a></li><li><a href=\"\">display</a></li><li><a href=\"\">Block&#32;and&#32;Inline&#32;Layout&#32;in&#32;Normal&#32;Flow</a></li></ol></p></div>"


    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
    div []
        [ p []
            [ text "Cryptids of Cornwall:" ]
        , dl []
            [ dt []
                [ text "Beast of Bodmin" ]
            , dd []
                [ text "A large feline inhabiting Bodmin Moor." ]
            , dt []
                [ text "Morgawr" ]
            , dd []
                [ text "A sea serpent." ]
            , dt []
                [ text "Owlman" ]
            , dd []
                [ text "A giant owl-like creature." ]
            ]
        ]
        |> toTextHtml
    --> "<div><p>Cryptids&#32;of&#32;Cornwall:</p><dl><dt>Beast&#32;of&#32;Bodmin</dt><dd>A&#32;large&#32;feline&#32;inhabiting&#32;Bodmin&#32;Moor.</dd><dt>Morgawr</dt><dd>A&#32;sea&#32;serpent.</dd><dt>Owlman</dt><dd>A&#32;giant&#32;owl-like&#32;creature.</dd></dl></div>"



    div []
        [ h1 [] [ text "Block-level elements" ]
        , p []
            [ text "In this article, we'll examine HTML block-level elements and how they differ from "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements" ] [ text "inline-level elements" ]
            , text "."
            ]
        , p []
            [ text "HTML ("
            , b [] [ text "Hypertext Markup Language" ]
            , text ") elements ... by CSS in the "
            , a [ href "https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout" ] [ text "Flow Layout" ]
            , text ". A Block-level element occupies ... contents, thereby creating a \"block\"."
            , aside []
                [ strong [] [ text "Note:" ]
                , text " A block-level element always starts on a new line and ... as it can)."
                ]
            , h3 [] [ text "See also" ]
            , ol []
                [ li [] [ a [ href "" ] [ text "Inline elements" ] ]
                , li [] [ a [ href "" ] [ text "display" ] ]
                , li [] [ a [ href "" ] [ text "Block and Inline Layout in Normal Flow" ] ]
                ]
            ]
        ]
        |> toTextPlain defaultTextPlainConfig
    --> String.join "\n"
    -->     [ "Block-level elements"
    -->     , ""
    -->     , "In this article, we'll examine HTML block-level elements and how they differ from inline-level elements https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements ."
    -->     , ""
    -->     , "HTML (Hypertext Markup Language) elements ... by CSS in the Flow Layout https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Flow_Layout . A Block-level element occupies ... contents, thereby creating a \"block\"."
    -->     , ""
    -->     , "Note: A block-level element always starts on a new line and ... as it can)."
    -->     , ""
    -->     , "See also"
    -->     , ""
    -->     , "    1. Inline elements"
    -->     , ""
    -->     , "    2. display"
    -->     , ""
    -->     , "    3. Block and Inline Layout in Normal Flow"
    -->     ]


    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
    div []
        [ p []
            [ text "Cryptids of Cornwall:" ]
        , dl []
            [ dt []
                [ text "Beast of Bodmin" ]
            , dd []
                [ text "A large feline inhabiting Bodmin Moor." ]
            , dt []
                [ text "Morgawr" ]
            , dd []
                [ text "A sea serpent." ]
            , dt []
                [ text "Owlman" ]
            , dd []
                [ text "A giant owl-like creature." ]
            ]
        ]
        |> toTextPlain defaultTextPlainConfig
    --> String.join "\n"
    -->     [ "Cryptids of Cornwall:"
    -->     , ""
    -->     , "Beast of Bodmin"
    -->     , ""
    -->     , "    A large feline inhabiting Bodmin Moor."
    -->     , ""
    -->     , "Morgawr"
    -->     , ""
    -->     , "    A sea serpent."
    -->     , ""
    -->     , "Owlman"
    -->     , ""
    -->     , "    A giant owl-like creature."
    -->     ]

    div []
        [ text "hi"
        , p [] [ text "hello" ]
        , text "world"
        , blockquote []
            [ text "alpha"
            , br [] []
            , text "beta"
            , br [] []
            , text "charlie"
            ]
        ]
        |> toTextPlain defaultTextPlainConfig
    --> String.join "\n"
    -->     [ "hi"
    -->     , ""
    -->     , "hello"
    -->     , ""
    -->     , "world"
    -->     , ""
    -->     , "    alpha"
    -->     , "    beta"
    -->     , "    charlie"
    -->     ]

-}

import Array exposing (Array)
import ElmEscapeHtml
import Html
import Html.Attributes
import Html.Events
import Html.Keyed
import Html.Parser
import HtmlData exposing (..)
import HtmlData.Attributes exposing (..)
import Json.Encode
import VirtualDom


{-| Returns `String` in `text/html` format, suitable for use in email or static browser rendering.

    import HtmlData exposing (..)
    import HtmlData.Attributes exposing (..)

    div [ classList
            [ ("hello", True)
            , ("world", True )
            , ("there", False )
            ]
        ]
        [ button [ id "Decrement", name "buttonDecrement" ] [ text "-" ]
        , div [] [ text ("Hello " ++ String.fromInt 1999) ]
        , button [ id "Increment", name "buttonIncrement" ] [ text "+" ]
        ]
        |> toTextHtml
    --> "<div class=\"hello&#32;world\"><button id=\"Decrement\" name=\"buttonDecrement\">-</button><div>Hello&#32;1999</div><button id=\"Increment\" name=\"buttonIncrement\">&#43;</button></div>"

    text "<bad> content"
    |> toTextHtml
    --> "&lt;bad&gt;&#32;content"

-}
toTextHtml : Html msg -> String
toTextHtml html =
    case html of
        Text string ->
            escapeHtml string

        Element name attrs children ->
            [ "<" :: name :: List.map texthtmlFromAttr attrs ++ [ ">" ]
            , List.map toTextHtml children
            , [ "</", name, ">" ]
            ]
                |> List.concat
                |> String.join ""

        KeyedElement name attrs children ->
            toTextHtml (Element name attrs (List.map Tuple.second children))

        LazyElement lazyf _ ->
            toTextHtml (lazyf ())


texthtmlFromAttr : Attribute msg -> String
texthtmlFromAttr attr =
    case attr of
        NoAttribute ->
            ""

        Attribute rawk rawv ->
            " " ++ escapeHtml rawk ++ "=\"" ++ escapeHtml rawv ++ "\""

        EventListener _ ->
            ""

        Property rawk value ->
            " " ++ escapeHtml rawk ++ "=" ++ Json.Encode.encode 0 value


{-| Config for converting html to text
-}
type alias TextPlainConfig msg =
    { textlinkFromHtml : List (Attribute msg) -> List (Html msg) -> String
    }


{-| Default TextPlainConfig provided out of the box

Note: there's a space added behind the string to prevent punctuations from
being confused as part of url.

    import HtmlData exposing (..)
    import HtmlData.Attributes exposing (..)

    defaultTextPlainConfig.textlinkFromHtml
        [ id "some-id"
        , href "https://example.com/url"
        , src "https://example.com/src"
        ]
        [ text "Hello World "
        , b [] [ text "Everyone!" ]
        , text " How are you?"
        ]
    --> "Hello World Everyone! How are you? https://example.com/url "

    defaultTextPlainConfig.textlinkFromHtml
        [ href "https://example.com/url" ]
        [ text "Click here"
        ]
    --> "Click here https://example.com/url "

    defaultTextPlainConfig.textlinkFromHtml
        [ href "https://example.com/url" ]
        [ text "See https://example.com/url"
        ]
    --> "See https://example.com/url "

    defaultTextPlainConfig.textlinkFromHtml
        [ href "https://example.com/url" ]
        [ text "https://example.com/url"
        ]
    --> "https://example.com/url "

-}
defaultTextPlainConfig : TextPlainConfig msg
defaultTextPlainConfig =
    { textlinkFromHtml = textlinkFromHtml
    }


{-| Returns `String` in `text/plain` format, suitable for use in email or console output.

    import HtmlData exposing (..)
    import HtmlData.Attributes exposing (..)


    div [ class "hello" ]
        [ p [] [ text "Hi Bob," ]
        , ol []
            [ li [] [ text "Do this" ]
            , li [] [ a [ href "https://example.com" ] [ text "Go here" ] ]
            ]
        ]
        |> toTextPlain defaultTextPlainConfig
    --> String.join "\n"
    -->     [ "Hi Bob,"
    -->     , ""
    -->     , "    1. Do this"
    -->     , ""
    -->     , "    2. Go here https://example.com "
    -->     ]

-}
toTextPlain : TextPlainConfig msg -> Html msg -> String
toTextPlain config element =
    toTextPlain_helper config 0 (always "") [ element ]


toTextPlain_helper : TextPlainConfig msg -> Int -> (Int -> String) -> List (Html msg) -> String
toTextPlain_helper config indent prefixEachChild htmlList =
    let
        each curr ( acc, inlineText ) =
            case curr of
                Text string ->
                    ( acc, inlineText ++ string )

                KeyedElement name attrs children ->
                    each (Element name attrs (List.map Tuple.second children)) ( acc, inlineText )

                LazyElement lazyf _ ->
                    each (lazyf ()) ( acc, inlineText )

                Element "a" attrs children ->
                    ( acc, inlineText ++ config.textlinkFromHtml attrs children )

                Element "ol" _ children ->
                    ( inlineText
                        |> asArrayUnless String.isEmpty
                        |> Array.append acc
                        |> Array.push
                            (children
                                |> toTextPlain_helper config (indent + 2) (\number -> String.fromInt (number + 1) ++ ". ")
                                |> String.append (prefix (indent + 2) ++ prefixEachChild (Array.length acc))
                            )
                    , ""
                    )

                Element "ul" _ children ->
                    ( inlineText
                        |> asArrayUnless String.isEmpty
                        |> Array.append acc
                        |> Array.push
                            (children
                                |> toTextPlain_helper config (indent + 2) (always "- ")
                                |> String.append (prefix (indent + 2) ++ prefixEachChild (Array.length acc))
                            )
                    , ""
                    )

                Element "br" _ _ ->
                    ( acc, inlineText ++ "\n" ++ prefix indent )

                Element _ _ children ->
                    if isIndented curr then
                        ( inlineText
                            |> asArrayUnless String.isEmpty
                            |> Array.append acc
                            |> Array.push
                                (children
                                    |> toTextPlain_helper config (indent + 4) (always "")
                                    |> String.append (prefix (indent + 4) ++ prefixEachChild (Array.length acc))
                                )
                        , ""
                        )

                    else if isBlockElement curr then
                        ( inlineText
                            |> asArrayUnless String.isEmpty
                            |> Array.append acc
                            |> Array.push (prefix indent ++ prefixEachChild (Array.length acc) ++ toTextPlain_helper config indent (always "") children)
                        , ""
                        )

                    else
                        ( acc, inlineText ++ toTextPlain_helper config indent (always "") children )
    in
    htmlList
        |> List.foldl each ( Array.empty, "" )
        |> (\( acc, inlineText ) ->
                inlineText
                    |> asArrayUnless String.isEmpty
                    |> Array.append acc
                    |> Array.toList
                    |> String.join ("\n\n" ++ prefix indent)
           )


prefix : Int -> String
prefix number =
    String.padLeft number ' ' ""


asArrayUnless : (String -> Bool) -> String -> Array String
asArrayUnless unless last =
    if unless last then
        Array.empty

    else
        Array.fromList [ last ]


textlinkFromHtml : List (Attribute msg) -> List (Html msg) -> String
textlinkFromHtml attrs children =
    let
        linkSuffix =
            List.filterMap
                (\attr ->
                    case attr of
                        Attribute "href" string ->
                            -- trailing space is by design
                            Just (string ++ " ")

                        Attribute _ _ ->
                            Nothing

                        NoAttribute ->
                            Nothing

                        EventListener _ ->
                            Nothing

                        Property _ _ ->
                            Nothing
                )
                attrs
                |> String.join ""

        linkContent =
            toTextPlain_helper { textlinkFromHtml = textlinkFromHtml } 0 (always "") children
                |> String.replace (String.trim linkSuffix) ""
                |> String.trim
    in
    [ linkContent, linkSuffix ]
        |> List.filter (\string -> String.trim string /= "")
        |> String.join " "



--


{-| Config for sanitization of content (element and attributes)
-}
type alias SanitizeConfig =
    { urlAttributes : List String
    , removedAttributes : List String
    , isAllowedUrl : String -> Bool
    }


{-| Default SanitizeConfig provided out of the box
-}
defaultSanitizeConfig : SanitizeConfig
defaultSanitizeConfig =
    { urlAttributes =
        --  https://stackoverflow.com/a/2725168
        [ "action"
        , "archive"
        , "background"
        , "cite"
        , "classid"
        , "codebase"
        , "content"
        , "data"
        , "dynsrc"
        , "formaction"
        , "href"
        , "icon"
        , "longdesc"
        , "lowsrc"
        , "manifest"
        , "poster"
        , "profile"
        , "src"
        , "srcset"
        , "usemap"
        ]
    , removedAttributes =
        [ "style"

        -- and some from urlAttributes
        , "action"
        , "archive"
        , "background"
        , "cite"
        , "classid"
        , "codebase"
        , "content"
        , "data"
        , "dynsrc"
        , "formaction"
        , "icon"
        , "longdesc"
        , "lowsrc"
        , "manifest"
        , "poster"
        , "profile"
        , "srcset"
        , "usemap"
        ]
    , isAllowedUrl =
        \urlString ->
            String.startsWith "http://" urlString
                || String.startsWith "https://" urlString
    }


{-| Given some `String`, run it through a sanitizer and get back safe `String` that we can use as `text/html`

    sanitize defaultSanitizeConfig """<h1 class="javascript:yo"> hello </h1>"""
    --> Just "<h1 class=\"javascript:yo\">&#32;hello&#32;</h1>"

    sanitize defaultSanitizeConfig """<a onclick='yo' data-other='yo' href="javascript :alert('Hi')">Cli>ckMe</a><script>alert("hello");</script>"""
    --> Just "<a data-other=\"yo\">Cli&gt;ckMe</a>"

    sanitize defaultSanitizeConfig """<b onmouseover=alert('Wufff!')>click me!</b>"""
    --> Nothing

    sanitize defaultSanitizeConfig """blah"/><script>alert("hello");</script>"""
    --> Just "blah&quot;/&gt;"

    sanitize defaultSanitizeConfig """<b onmouseover=alert(‘XSS!‘)></b>"""
    --> Just "<b></b>"

    sanitize defaultSanitizeConfig """<body style="javascript:yo" onload=alert(‘something’)></body>"""
    --> Just "<body></body>"

    sanitize defaultSanitizeConfig """<script>alert("hello");</script>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<scr<script>ipt>alert(‘XSS’)</script>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<SCRIPT>yo</SCRIPT>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<IMG SRC=j&#X41vascript:alert('test2')>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<IMG SRC="j&#X41vascript:alert('test2')">"""
    --> Just "<img>"

    sanitize defaultSanitizeConfig """<a onclick='yo' href="javascript :alert('Hi')">ClickMe</a><scr<script>ipt>alert("hello");</script>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<img src="data:text/html;base64,PHNjcmlwdD5hbGVydCgndGVzdDMnKTwvc2NyaXB0Pg">"""
    --> Just "<img>"

    sanitize defaultSanitizeConfig """< h1>strict</h1>"""
    --> Nothing

    sanitize defaultSanitizeConfig """<h1>strict</ h1>"""
    --> Nothing

    sanitize defaultSanitizeConfig ""
    --> Nothing

-}
sanitize : SanitizeConfig -> String -> Maybe String
sanitize config rawHtml =
    Html.Parser.run rawHtml
        |> Result.toMaybe
        |> Maybe.map (List.filterMap (sanitizeNode config))
        |> Maybe.map (List.map Html.Parser.nodeToString)
        |> Maybe.map String.concat
        |> Maybe.andThen
            (\s ->
                if String.trim s == "" then
                    Nothing

                else
                    Just s
            )


sanitizeNode : SanitizeConfig -> Html.Parser.Node -> Maybe Html.Parser.Node
sanitizeNode config node =
    case node of
        Html.Parser.Text s ->
            Just (Html.Parser.Text (escapeHtml s))

        Html.Parser.Comment _ ->
            Nothing

        Html.Parser.Element rawName attr children ->
            case String.filter (\c -> Char.isAlphaNum c || c == '-') (normalize rawName) of
                "" ->
                    Nothing

                "style" ->
                    Nothing

                "script" ->
                    Nothing

                name ->
                    Just (Html.Parser.Element name (List.filterMap (sanitizeAttribute config) attr) (List.filterMap (sanitizeNode config) children))


sanitizeAttribute : SanitizeConfig -> Html.Parser.Attribute -> Maybe Html.Parser.Attribute
sanitizeAttribute config ( rawName, rawValue ) =
    let
        name =
            normalize rawName
    in
    if String.startsWith "on" name then
        Nothing

    else if List.member name config.removedAttributes then
        Nothing

    else if List.member name config.urlAttributes && not (config.isAllowedUrl (normalize rawValue)) then
        Nothing

    else
        Just ( name, escapeHtml rawValue )


normalize : String -> String
normalize =
    String.trim << String.toLower


{-| <http://wonko.com/post/html-escaping>

    "<a href=\"/user/foo\" onmouseover=\"alert(1)\">foo\" onmouseover=\"alert(1)</a>"
    |> escapeHtml
    --> "&lt;a&#32;href&#61;&quot;/user/foo&quot;&#32;onmouseover&#61;&quot;alert&#40;1&#41;&quot;&gt;foo&quot;&#32;onmouseover&#61;&quot;alert&#40;1&#41;&lt;/a&gt;"

    "<a href='/user/foo' onmouseover='alert(1)'>foo' onmouseover='alert(1)</a>"
    |> escapeHtml
    --> "&lt;a&#32;href&#61;&#39;/user/foo&#39;&#32;onmouseover&#61;&#39;alert&#40;1&#41;&#39;&gt;foo&#39;&#32;onmouseover&#61;&#39;alert&#40;1&#41;&lt;/a&gt;"

-}
escapeHtml : String -> String
escapeHtml rawText =
    ElmEscapeHtml.escape rawText


isIndented : Html msg -> Bool
isIndented element =
    case element of
        Text _ ->
            False

        Element eleName _ _ ->
            List.member eleName
                [ "blockquote", "dd", "ol", "ul" ]

        KeyedElement name attrs children ->
            isIndented (Element name attrs (List.map Tuple.second children))

        LazyElement lazyf _ ->
            isIndented (lazyf ())


isBlockElement : Html msg -> Bool
isBlockElement element =
    case element of
        Text _ ->
            False

        Element eleName _ _ ->
            List.member eleName blockElements

        KeyedElement name attrs children ->
            isBlockElement (Element name attrs (List.map Tuple.second children))

        LazyElement lazyf _ ->
            isBlockElement (lazyf ())


{-| <https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements#elements>
-}
blockElements : List String
blockElements =
    [ "address"
    , "article"
    , "aside"
    , "blockquote"
    , "details"
    , "dialog"
    , "dd"
    , "div"
    , "dl"
    , "dt"
    , "fieldset"
    , "figcaption"
    , "figure"
    , "figcaption"
    , "footer"
    , "form"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "header"
    , "hgroup"
    , "hr"
    , "li"
    , "main"
    , "nav"
    , "ol"
    , "p"
    , "pre"
    , "section"
    , "table"
    , "ul"
    ]



--


{-| Converts into a regular [elm/html `Html msg`](https://package.elm-lang.org/packages/elm/html/1.0.0/Html)
-}
toElmHtml : Html msg -> Html.Html msg
toElmHtml htmlnode =
    case htmlnode of
        Text s ->
            Html.text s

        Element name attrs children ->
            Html.node name
                (attrsToElmHtml attrs)
                (List.map toElmHtml children)

        KeyedElement name attrs children ->
            Html.Keyed.node name
                (attrsToElmHtml attrs)
                (List.map (Tuple.mapSecond toElmHtml) children)

        LazyElement _ lazyHtml ->
            lazyHtml ()


attrsToElmHtml : List (Attribute msg) -> List (Html.Attribute msg)
attrsToElmHtml attrList =
    List.foldr
        (\attr acc ->
            case attr of
                Attribute key string ->
                    VirtualDom.attribute key string :: acc

                NoAttribute ->
                    acc

                EventListener l ->
                    listenerToElmHtml l :: acc

                Property key value ->
                    VirtualDom.property key value :: acc
        )
        []
        attrList


listenerToElmHtml : EventListener msg -> Html.Attribute msg
listenerToElmHtml l =
    case l of
        On s d ->
            Html.Events.on s d

        OnInput msg ->
            Html.Events.onInput msg

        OnCheck msg ->
            Html.Events.onCheck msg

        StopPropagationOn s d ->
            Html.Events.stopPropagationOn s d

        PreventDefaultOn s d ->
            Html.Events.preventDefaultOn s d

        Custom s d ->
            Html.Events.custom s d



--


{-| Converts from [`Html.Parser.Node`](https://package.elm-lang.org/packages/hecrj/html-parser/latest/Html-Parser#Node) into `HtmlData.Html`

We could achieve `String -> List (HtmlData.Html msg)` by

1.  combining with [`Html.Parser.run`](https://package.elm-lang.org/packages/hecrj/html-parser/latest/Html-Parser#run)

2.  adding fallback value for error

Like this

    import Html.Parser
    import HtmlData exposing (..)
    import HtmlData.Attributes exposing (..)

    fromString : String -> List (Html msg)
    fromString str =
        case Html.Parser.run str of
            Err err ->
                [ text (Debug.toString err) ]
            --
            Ok nodes ->
                fromHtmlParserNodes nodes

    fromString "<p class=\"hello world\"><b>young</b> and <em>dangerous</em></p>"
    --> [Element "p" [Attribute "class" "hello world"] [Element "b" [] [Text "young"],Text " and ",Element "em" [] [Text "dangerous"]]]

-}
fromHtmlParserNodes : List Html.Parser.Node -> List (Html msg)
fromHtmlParserNodes nodes =
    List.map fromHtmlParserNodes_helper nodes


fromHtmlParserNodes_helper : Html.Parser.Node -> Html msg
fromHtmlParserNodes_helper node =
    case node of
        Html.Parser.Element name attrs children ->
            Element name (List.map fromHtmlParserAttr attrs) (List.map fromHtmlParserNodes_helper children)

        Html.Parser.Text s ->
            text s

        Html.Parser.Comment _ ->
            text ""


fromHtmlParserAttr : ( String, String ) -> Attribute msg
fromHtmlParserAttr ( k, v ) =
    Attribute k v
