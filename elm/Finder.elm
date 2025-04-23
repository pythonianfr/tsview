module Finder exposing
    ( find
    , rawtsdescdecoder
    , SeriesModel
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


type alias SeriesModel =
    { items : List S.Series
    , query : Expr
    , errors : List String
    }


empty =
    { items = []
    , query = Expression [ Atom <| Symbol "by.everything" ]
    , errors = []
    }


-- codec

rawtsdescdecoder =
    JD.list S.seriesdecoder


-- http + events

type Msg
    = ReceivedSeries (Result Http.Error String)


update msg model =
    let
        onerror err =
            { model | errors = List.append model.errors [ err ] }

    in
    case msg of
        ReceivedSeries (Ok raw) ->
            case JD.decodeString rawtsdescdecoder raw of
                Ok seriesdesc ->
                    { model | series = seriesdesc }

                Err err ->
                    onerror (JD.errorToString err)

        ReceivedSeries (Err err) ->
            onerror (U.unwraperror err)


find urlprefix event query =
    Http.get
        { expect = Http.expectString event
        , url =
            UB.crossOrigin urlprefix
                [ "api", "series", "find" ]
                [ UB.string "query" <| Lisp.serialize query ]
        }
