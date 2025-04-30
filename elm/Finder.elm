module Finder exposing
    ( empty
    , find
    , Msg(..)
    , rawtsdescdecoder
    , Model
    , update
    )

import Http
import Json.Decode as JD
import Lisp exposing
    ( Atom(..)
    , Expr(..)
    )
import Series as S
import Url.Builder as UB
import Util as U


-- model

type Error
    = Error String


type alias Model =
    { items : List S.Series
    , errors : List String
    }


empty =
    { items = []
    , errors = []
    }


-- codec

rawtsdescdecoder =
    JD.list S.seriesdecoder


-- http + events

type Msg
    = ReceivedSeries (Result Http.Error String)
    | ReceivedGroups (Result Http.Error String)


update msg model =
    let
        onerror err =
            { model | errors = List.append model.errors [ err ] }

    in
    case msg of
        ReceivedSeries (Ok raw) ->
            case JD.decodeString rawtsdescdecoder raw of
                Ok seriesdesc ->
                    { model | items = seriesdesc }

                Err err ->
                    onerror (JD.errorToString err)

        ReceivedSeries (Err err) ->
            onerror (U.unwraperror err)

        ReceivedGroups (Ok raw) ->
            case JD.decodeString rawtsdescdecoder raw of
                Ok groupsdesc ->
                    { model | items = groupsdesc }

                Err err ->
                    onerror (JD.errorToString err)

        ReceivedGroups (Err err) ->
            onerror (U.unwraperror err)


find baseurl dtype event query sources limit =
    Http.get
        { expect = Http.expectString event
        , url =
            UB.crossOrigin baseurl
                [ "api", dtype, "find" ]
                [ UB.string "query" query
                , UB.string "sources" <| String.join "," sources
                , UB.int "limit" limit
                ]
        }
