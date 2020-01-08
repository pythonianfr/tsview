module Catalog exposing (RawSeriesCatalog, SeriesCatalog, seriesFromCatalog, kindsFromCatalog)

import Dict exposing(Dict, fromList, keys, values)

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
        List.map makeSeries (List.concat (values rawCatalog))


kindsFromCatalog rawCatalog =
    let
        makeSeriesTuple: List String -> (String, String)
        makeSeriesTuple rawList =
            case rawList of
                [a,b] -> (a, b)
                _ -> ("<nosuchseries>", "<nosuchkind>")
    in
        fromList (List.map makeSeriesTuple (List.concat (values rawCatalog)))


type alias SeriesCatalog =
    { series : List String
    , seriesBySource : List String -- Dict String (Set Series)
    , seriesByKind : Dict String String -- Dict String (Set Series)
    }
