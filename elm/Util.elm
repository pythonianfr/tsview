module Util exposing
    ( adderror
    , first
    , nocmd
    , snd
    , tovirtualdom
    , unwraperror
    )

import Html
import Html.Parser
import Html.Parser.Util
import Http


nocmd model = ( model, Cmd.none )


first = Tuple.first
snd = Tuple.second


adderror model error =
    { model | errors = List.append model.errors [error] }


unwraperror : Http.Error -> String
unwraperror resp =
    case resp of
        Http.BadUrl x -> "bad url: " ++ x
        Http.Timeout -> "the query timed out"
        Http.NetworkError -> "there was a network error"
        Http.BadStatus val -> "we got a bad status answer: " ++ String.fromInt val
        Http.BadBody body -> "we got a bad body: " ++ body


tovirtualdom : String -> String -> List (Html.Html msg)
tovirtualdom html errmsg =
    case Html.Parser.run html of
        Ok nodes ->
            Html.Parser.Util.toVirtualDom nodes
        Err x ->
            [Html.div [] [ Html.text errmsg ] ]
