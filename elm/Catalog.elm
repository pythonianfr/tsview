module Catalog exposing
    (Model
    , Msg(..)
    , Error
    , empty
    , viewError
    , get
    , newseries
    , update
    , removeSeries
    )

import Common
import Dict exposing(Dict)
import Html exposing (Html, div, text, span)
import Http
import Json.Decode as D
import List.Extra exposing (unique)
import Set exposing (Set)
import Url.Builder as UB
import Util as U


type alias RawSeries =
    Dict String (List (String, String))


type Error
    = Error String


type alias Model =
    { series : List String
    , seriesBySource : Dict String (Set String)
    , seriesByKind : Dict String (Set String)
    , groups : List String
    , groupsBySource : Dict String (Set String)
    , groupsByKind : Dict String (Set String)
    , errors : List String
    }


empty =
    Model [] Dict.empty Dict.empty [] Dict.empty Dict.empty []


newseries model raw =
    { model
          | series = (buildseries raw)
          , seriesBySource = (buildsources raw)
          , seriesByKind = (buildkinds raw)
    }


newgroups model raw =
    { model
          | groups = (buildseries raw)
          , groupsBySource = (buildsources raw)
          , groupsByKind = (buildkinds raw)
    }



type Msg
    = ReceivedSeries (Result Http.Error String)
    | ReceivedGroups (Result Http.Error String)


-- catalog update

update msg model =
    let
        onerror err =
            { model | errors = List.append model.errors [ err ] }

    in
    case msg of
        ReceivedSeries (Ok x) ->
            case decodecatalog x of
                Ok seriescat ->
                    newseries model seriescat

                Err err ->
                    onerror (D.errorToString err)

        ReceivedSeries (Err err) ->
            onerror (U.unwraperror err)

        ReceivedGroups (Ok x) ->
            case decodecatalog x of
                Ok groupscat ->
                    newgroups model groupscat

                Err err ->
                    onerror (D.errorToString err)

        ReceivedGroups (Err err) ->
            onerror (U.unwraperror err)


-- view

viewError : Error -> Html msg
viewError error =
    let
        bold x =
            span [ ] [ text x ]
    in
        case error of
            Error x ->
                div [] [ bold "Catalog error", text x ]


-- catalog building

buildseries raw =
    List.map Tuple.first (List.concat (Dict.values raw))


groupby : List (String, String)
        -> ((String, String) -> String)
        -> ((String, String) -> String)
        -> Dict String (Set String)
groupby list itemaccessor keyaccessor =
    let
        allkeys = unique (List.map keyaccessor list)
        filterItemsByKey items key =
            List.map itemaccessor (List.filter (\x -> (==) key (keyaccessor x)) items)
        makeDictEntry key =
            Tuple.pair key (Set.fromList (filterItemsByKey list key))
    in
        Dict.fromList (List.map makeDictEntry allkeys)


buildkinds : RawSeries -> Dict String (Set String)
buildkinds raw =
    groupby (List.concat (Dict.values raw)) Tuple.first Tuple.second


buildsources raw =
    let
        namesbysource source =
            List.map Tuple.first (Maybe.withDefault [] (Dict.get source raw))
        makedictentry source =
            Tuple.pair source (Set.fromList (namesbysource source))
    in
         Dict.fromList (List.map makedictentry (Dict.keys raw))


removeSeries name catalog =
    let
        removeItem x xs = List.filter ((/=) x) xs
    in
        { catalog | series = removeItem name catalog.series }


-- catalog fetching

decodetuple =
    D.map2 Tuple.pair
        (D.index 0 D.string)
        (D.index 1 D.string)


decodecatalog rawcat =
    D.decodeString (D.dict (D.list decodetuple)) rawcat


get urlprefix dtype allsources event =
    Http.get
        { expect = Http.expectString event
        , url =
            UB.crossOrigin urlprefix
                [ "api", dtype, "catalog" ]
                [ UB.int "allsources" allsources ]
        }
