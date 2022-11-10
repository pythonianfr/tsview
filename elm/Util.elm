module Util exposing
    ( adderror
    , bool2int
    , cleanupdate
    , dateof
    , filterbyformula
    , first
    , fragmentsmatcher
    , getformulas
    , nocmd
    , pygmentyze
    , snd
    , tovirtualdom
    , unwraperror
    )

import Dict exposing (Dict)
import Html
import Html.Parser
import Html.Parser.Util
import Http
import Url.Builder as UB


nocmd model = ( model, Cmd.none )


bool2int b =
    if b then 1 else 0


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


-- common to Search and Cache
-- helps the series filtering

fragmentsmatcher query item =
    -- predicate for item containing all query space-separated parts
    List.all
        (\queryfragment -> String.contains queryfragment item)
        (String.split " " query)


filterbyformula formulas filterme query =
    let
        formula name =
            Maybe.withDefault "" <| Dict.get name formulas
        informula name =
            -- formula part -> name
            fragmentsmatcher query <| formula name
    in
    List.filter informula filterme


-- formulas

getformulas baseurl event =
    Http.get
        { expect =
              Http.expectString event
        , url =
            UB.crossOrigin baseurl
                [ "tssearch", "allformula" ] []
        }


pygmentyze model formula callback =
    Http.post
        { url =
              UB.crossOrigin
              model.baseurl
              [ "tsformula", "pygmentize" ]
              []
        , body = Http.stringBody "text/plain" formula
        , expect = Http.expectString callback
        }


-- dates

cleanupdate date =
    -- html dates are not really ISO compliant
    -- hence we have to remove a few bits to make them work
    -- in some widgets
    case String.split "+" date of
        head::_ ->
            case String.split "." head of
                newhead::_ ->
                    newhead
                [] -> ""
        [] -> ""


dateof strdate =
    case String.split "T" strdate of
        head::_ ->
            head
        [] -> ""
