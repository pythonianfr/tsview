module Catalog exposing
    (Model
    , Msg
    , Error
    , viewError
    , get
    , new
    , update
    , removeSeries
    )

import Common
import Dict exposing(Dict)
import Html.Styled exposing (Html, div, text, span)
import Http
import Json.Decode as D
import List.Extra exposing (unique)
import Set exposing (Set)
import Tachyons.Classes as T
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
    , errors : List String
    }


type Msg
    = Received (Result Http.Error String)


-- catalog update

update msg model =
    case msg of
        Received (Ok x) ->
            case decodecatalog x of
                Ok cat -> new cat
                Err err ->
                    Model [] Dict.empty Dict.empty [ D.errorToString err ]

        Received (Err err) ->
            Model [] Dict.empty Dict.empty [ U.unwraperror err ]


-- view

viewError : Error -> Html msg
viewError error =
    let
        bold x =
            span [ Common.classes [ T.b, T.mr4 ] ] [ text x ]
    in
        case error of
            Error x ->
                div [] [ bold "Catalog error", text x ]


-- catalog building

newseries : RawSeries -> List String
newseries raw =
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


newkinds : RawSeries -> Dict String (Set String)
newkinds raw =
    groupby (List.concat (Dict.values raw)) Tuple.first Tuple.second


newsources raw =
    let
        namesbysource source =
            List.map Tuple.first (Maybe.withDefault [] (Dict.get source raw))
        makedictentry source =
            Tuple.pair source (Set.fromList (namesbysource source))
    in
         Dict.fromList (List.map makedictentry (Dict.keys raw))


new : RawSeries -> Model
new raw =
    let
        series = newseries raw
        seriesBySource = newsources raw
        seriesByKind = newkinds raw
    in
        Model series seriesBySource seriesByKind []


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


get : String -> Int -> Cmd Msg
get urlprefix allsources =
    Http.get
        { expect = Http.expectString Received
        , url =
            UB.crossOrigin urlprefix
                [ "api", "series", "catalog" ]
                [ UB.int "allsources" allsources ]
        }
