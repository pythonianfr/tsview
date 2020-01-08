module Catalog exposing (RawSeriesCatalog, SeriesCatalog, getCatalog, buildCatalog, removeSeries)

import Common
import Dict exposing(Dict)
import Set exposing (Set)
import Http
import Json.Decode as Decode
import List.Extra exposing (unique)
import Url.Builder as UB


type alias RawSeriesCatalog =
    Dict String (List (String, String))


decodeTuple =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.string)
        (Decode.index 1 Decode.string)


seriesFromCatalog rawCatalog =
    List.map Tuple.first (List.concat (Dict.values rawCatalog))


kindsFromCatalog rawCatalog =
    let
        allserieskinds = List.concat (Dict.values rawCatalog)
        allkinds = unique (List.map Tuple.second allserieskinds)

        filternamesbykind kind xs =
            List.map Tuple.first (List.filter (\x -> (==) kind (Tuple.second x)) xs)

        makedictentry kind =
            Tuple.pair kind (Set.fromList (filternamesbykind kind allserieskinds))
    in
        Dict.fromList (List.map makedictentry allkinds)


sourcesFromCatalog rawCatalog =
    let
        namesbysource source =
            List.map Tuple.first (Maybe.withDefault [] (Dict.get source rawCatalog))
        makedictentry source =
            Tuple.pair source (Set.fromList (namesbysource source))
    in
         Dict.fromList (List.map makedictentry (Dict.keys rawCatalog))


buildCatalog : RawSeriesCatalog -> SeriesCatalog
buildCatalog rawCatalog =
    let
        series = seriesFromCatalog rawCatalog
        seriesBySource = sourcesFromCatalog rawCatalog
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
              (Decode.dict (Decode.list decodeTuple))
        , url =
            UB.crossOrigin urlPrefix
                [ "api", "series", "catalog" ]
                []
        }



type alias SeriesCatalog =
    { series : List String
    , seriesBySource : Dict String (Set String)
    , seriesByKind : Dict String (Set String)
    }
