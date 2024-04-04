port module Horizon exposing
    ( DataInCache
    , Horizon
    , HorizonModel
    , Offset
    , dataInCacheDecoder
    , defaultHorizon
    , horizons
    , horizonwidget
    , saveToLocalStorage
    , savedDataInCache
    , updateDataInCache
    , updateHorizon
    , updateHorizonModel
    , updateOffset
    )

import Dict exposing (Dict)
import Either exposing (Either(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Maybe.Extra as Maybe
import OrderedDict as OD
import Util as U


type alias Offset =
    Either Int Int


type alias HorizonModel v =
    { offset : Int
    , horizon : Horizon
    , inferredFreq : Bool
    , mindate : String
    , maxdate : String
    , timeSeries : Dict String v
    , timeZone : String
    }


type alias Horizon =
    { key : Maybe String }


type alias DataInCache =
    { horizon : Maybe String
    , timeZone : String
    , inferredFreq : Bool
    }


type alias HorizonMsg msg =
    { inferredFreqMsg : ( Bool -> msg )
    , timeZoneMsg : ( String -> msg )
    , offsetMsg : ( Offset -> msg )
    , timeDeltaMsg : ( Horizon -> msg )
    }


port saveToLocalStorage : DataInCache -> Cmd msg


port savedDataInCache : (String -> msg) -> Sub msg


dataInCacheDecoder : D.Decoder DataInCache
dataInCacheDecoder =
    D.map3 DataInCache
        (D.field "horizon" (D.nullable D.string))
        (D.field "timeZone" D.string)
        (D.field "inferredFreq" D.bool)


defaultHorizon : String
defaultHorizon =
    "2 weeks"


horizons : OD.OrderedDict String String
horizons =  OD.fromList
    [ ( "1 week"
      , """(horizon
         #:date (now)
         #:past (delta #:days -7)
         #:future (delta #:days 7)
         #:offset {offset})
         """
      )
     , ( "2 weeks"
       , """(horizon
          #:date (now)
          #:past (delta #:days -14)
          #:future (delta #:days 14)
          #:offset {offset})
          """
       )
    , ( "1 month"
      , """
         (horizon #:date (now)
         #:past (delta #:months -1)
         #:future (delta #:months 1)
         #:offset {offset})
         """
      )
    , ( "3 months"
      , """
         (horizon #:date (now)
         #:past (delta #:months -3)
         #:future (delta #:months 3)
         #:offset {offset})
         """
      )
    , ( "1 year"
      , """
         (horizon #:date (now)
         #:past (delta #:years -1)
         #:future (delta #:years 1)
         #:offset {offset})
         """
      )
    ]


updateOffset : Int -> HorizonModel v -> HorizonModel v
updateOffset newOffset model =
    { model | offset = newOffset }


updateHorizon : Horizon -> HorizonModel v -> HorizonModel v
updateHorizon horizon model =
    { model
        | horizon = horizon
        , offset = 0
    }


updateDataInCache : DataInCache -> HorizonModel v -> HorizonModel v
updateDataInCache dataInCache model =
    { model
        | horizon = { key = dataInCache.horizon }
        , inferredFreq = dataInCache.inferredFreq
        , timeZone = dataInCache.timeZone
    }


updateHorizonModel : HorizonModel v -> Dict String v -> HorizonModel v
updateHorizonModel model val =
    let
        tsBounds = formatBoundDates val
    in { model
           | mindate = Tuple.first tsBounds
           , maxdate = Tuple.second tsBounds
           , timeSeries = val
       }


formatBoundDates : Dict String v -> (String, String)
formatBoundDates val =
    let
        dates = Dict.keys val
        minappdate =
            case dates of
                head::_ -> U.cleanupdate head
                []  -> ""
        maxappdate = U.cleanupdate
                     <| Maybe.withDefault ""
                     <| List.maximum dates
    in ( U.dateof minappdate, U.dateof maxappdate )


buttonArrow : String  -> HorizonMsg msg -> String -> H.Html msg
buttonArrow direction horizonMsg className =
    let
        arrow = if direction == "left" then Left else Right
    in
    H.button
        [ HA.class className ]
        [ H.i
            [ HA.class <| String.replace "{arrow}" direction "bi bi-arrow-{arrow}"
            , HE.onClick (horizonMsg.offsetMsg (arrow 1))
            ] [ ]
        ]


selectHorizon : HorizonModel v -> HorizonMsg msg -> H.Html msg
selectHorizon model horizonMsg =
    H.select
        [ HE.targetValue
            |> D.andThen readHorizon
            |> D.map horizonMsg.timeDeltaMsg
            |> HE.on "change"
        ]
        (List.map (renderhorizon model.horizon.key)
            <| "All" :: (OD.keys horizons)
        )


renderhorizon : Maybe String -> String -> H.Html msg
renderhorizon selectedhorizon horizon =
    H.option
        [ HA.value horizon
        , HA.selected <| Maybe.unwrap False ((==) horizon) selectedhorizon
        ]
        [ H.text horizon ]


readHorizon : String -> D.Decoder Horizon
readHorizon key = D.succeed <| Horizon <|
    if List.member key (OD.keys horizons) then
        Just key
    else
        Nothing


renderTimeZone : String -> String -> H.Html msg
renderTimeZone selectedhorizon timeZone =
    H.option
        [ HA.value timeZone
        , HA.selected (selectedhorizon == timeZone)
        ]
        [ H.text timeZone ]


divSelectTimeZone : HorizonModel v -> HorizonMsg msg -> H.Html msg
divSelectTimeZone model horizonMsg =
    let
        decodeTimeZone : String -> D.Decoder msg
        decodeTimeZone timeZone =
            D.succeed (horizonMsg.timeZoneMsg timeZone)

    in
    H.div
        [ ]
        [ H.select
              [ HE.on "change" (D.andThen decodeTimeZone HE.targetValue) ]
              (List.map (renderTimeZone model.timeZone) ["UTC", "CET"])
        ]


horizonwidget : HorizonModel v -> HorizonMsg msg -> String -> H.Html msg
horizonwidget model horizonMsg klass =
    H.div
        [ HA.class klass ]
        [ H.div
            []
            [ tzonedropdown model horizonMsg ]
        , H.div
            []
            [ buttonArrow "left" horizonMsg "btn btn-outline-dark btn-sm" ]
        , H.div
            []
            [ H.text model.mindate ]
        , H.div
            []
            [ selectHorizon model horizonMsg ]
        , H.div
            []
            [ H.text model.maxdate ]
        , H.div
            []
            [ buttonArrow "right" horizonMsg "btn btn-outline-dark btn-sm" ]
        , H.div
            []
            [ inferredfreqswitch model horizonMsg ]
        ]


inferredfreqswitch : HorizonModel v -> HorizonMsg msg -> H.Html msg
inferredfreqswitch model horizonMsg =
    H.div
        [ HA.class "custom-control custom-switch"]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "flexSwitchCheckDefault"
            , HA.checked model.inferredFreq
            , HE.onCheck horizonMsg.inferredFreqMsg
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "flexSwitchCheckDefault"
            ]
            [ H.text "inferred freq" ]
        ]


tzonedropdown : HorizonModel v -> HorizonMsg msg -> H.Html msg
tzonedropdown model horizonMsg =
    let
        decodeTimeZone : String -> D.Decoder msg
        decodeTimeZone timeZone =
            D.succeed (horizonMsg.timeZoneMsg timeZone)

    in
    H.select
        [ HE.on "change" (D.andThen decodeTimeZone HE.targetValue) ]
        (List.map (renderTimeZone model.timeZone) ["UTC", "CET"])
