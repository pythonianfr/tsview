module Icons exposing (Icon, buildSvg, getIcons)

import Dict exposing (Dict)
import Html exposing ( Html )
import Svg exposing (path, svg)
import Svg.Attributes
    exposing
        ( d
        , fill
        , fillRule
        , viewBox
        )
import Http
import Json.Decode exposing (Decoder)
import Json.Decode as JD

type alias Icon = List Path

type alias Path =
    { d: String
    , fillRule: Maybe String
    }

buildSvg: Dict String Icon -> String -> Html msg
buildSvg icones iconeName =
    let
        icone =
            Maybe.withDefault
                []
                (Dict.get iconeName icones)
    in
    svg
        [ viewBox "0 0 16 16"
        , fill "currentColor"
        ]
        (List.map
            (\ipath -> buildSvgPath ipath)
            icone
        )

buildSvgPath: Path -> Html msg
buildSvgPath ipath =
    case ipath.fillRule of
        Nothing ->
            path [ d ipath.d ] []

        Just rule ->
            path [ fillRule rule, d ipath.d ] []

getIcons: String -> ((Result Http.Error (Dict String Icon)) -> msg) -> Cmd msg
getIcons baseUrl msgBuilder =
    Http.get
        { url = baseUrl ++ "icons"
        , expect = Http.expectJson msgBuilder iconesDecoder
        }

iconesDecoder: Decoder (Dict String Icon)
iconesDecoder = JD.dict decodeIcone

decodeIcone: Decoder Icon
decodeIcone = JD.list decodePath


decodePath: Decoder Path
decodePath =
    JD.map2 Path
        ( JD.field "d" JD.string )
        ( JD.maybe (JD.field "fillRule" JD.string ) )