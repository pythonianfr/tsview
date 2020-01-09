module Catalog exposing (RawSeriesCatalog, SeriesCatalog, getCatalog, buildCatalog, removeSeries)

import Common
import Dict exposing(Dict)
import Set exposing (Set)
import Http
import Json.Decode as Decode
import Url.Builder as UB


type alias RawSeriesCatalog =
    Dict String (List (List String))


seriesFromCatalog rawCatalog =
    let
        makeSeries: List String -> String
        makeSeries rawlist =
            case rawlist of
                [a,_] -> a
                _ -> "<nosuchseries>"
    in
        List.map makeSeries (List.concat (Dict.values rawCatalog))


kindsFromCatalog rawCatalog =
    let
        makeSeriesTuple: List String -> (String, String)
        makeSeriesTuple rawList =
            case rawList of
                [a,b] -> (a, b)
                _ -> ("<nosuchseries>", "<nosuchkind>")
    in
        Dict.fromList (List.map makeSeriesTuple (List.concat (Dict.values rawCatalog)))


buildCatalog : RawSeriesCatalog -> SeriesCatalog
buildCatalog rawCatalog =
    let
        series = seriesFromCatalog rawCatalog
        seriesBySource = Dict.fromList []
        seriesByKind = kindsFromCatalog rawCatalog
    in
        SeriesCatalog series seriesBySource seriesByKind


removeSeries name catalog =
    let
        removeItem x xs = List.filter ((/=) x) xs
    in
        { catalog | series = removeItem name catalog.series }


getCatalog urlPrefix expectcatalog =
    Http.get
        { expect =
              expectcatalog
              (Decode.dict (Decode.list (Decode.list (Decode.string))))
        , url =
            UB.crossOrigin urlPrefix
                [ "api", "series", "catalog" ]
                []
        }



type alias SeriesCatalog =
    { series : List String
    , seriesBySource : Dict String (Set String)
    , seriesByKind : Dict String String -- Dict String (Set Series)
    }
