module Horizon exposing
    ( Horizon
    , HorizonModel
    , Offset
    , defaultHorizon
    , horizonbtnGroup
    , horizons
    , updateHorizonModel
    )

import Dict exposing (Dict)
import Either exposing (Either(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Maybe.Extra as Maybe
import Util as U


type alias Offset =
    Either Int Int


type alias HorizonModel v =
    { offset : Int
    , offset_reached : Bool
    , horizon : Horizon
    , mindate : String
    , maxdate : String
    , timeSeries : Dict String v
    }


type alias Horizon =
    { key : Maybe String }


defaultHorizon : String
defaultHorizon =
        "2 weeks"


horizons : Dict String String
horizons =  Dict.fromList
    [Tuple.pair
        "2 weeks"
        """(horizon
            #:date (now)
            #:past (delta #:days -14)
            #:future (delta #:days 14)
            #:offset {offset})
        """
    , Tuple.pair
        "1 month"
        """
            (horizon #:date (now)
            #:past (delta #:days -30)
            #:future (delta #:days 30)
            #:offset {offset})
        """
    ]


updateHorizonModel : HorizonModel v -> Dict String v -> HorizonModel v
updateHorizonModel model val =
    if Dict.isEmpty val then
        let
            last_offset =
                if model.offset < 0 then
                    model.offset + 1
                else
                    model.offset - 1
        in { model
            | offset_reached = True
            , offset = last_offset}
    else
        let
            tsBounds = formatBoundDates val
        in { model
                | mindate = Tuple.first tsBounds
                , maxdate = Tuple.second tsBounds
                , offset_reached = False
                , timeSeries = val}


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
            in (U.dateof minappdate, U.dateof maxappdate)


offsetDisabledLeft : HorizonModel v -> Bool
offsetDisabledLeft {offset, offset_reached, horizon} =
    ((offset > 0) && offset_reached) || Maybe.isNothing horizon.key


offsetDisabledRight : HorizonModel v -> Bool
offsetDisabledRight {offset, offset_reached, horizon} =
    ((offset < 0) && offset_reached) || Maybe.isNothing horizon.key


buttonArrow : String -> Bool ->  ( Offset -> msg ) -> H.Html msg
buttonArrow direction disabled message =
    let
        arrow = if direction == "left" then Left else Right
    in
    H.button
        [ HA.class "btn btn-outline-dark btn-sm"
        , HA.disabled disabled
        ]
        [ H.i
            [ HA.class
                <| String.replace "{arrow}" direction "bi bi-arrow-{arrow}"
            , HE.onClick (message (arrow 1))
            ]
            [ ]
        ]


selectHorizon : HorizonModel v -> ( Horizon -> msg ) -> H.Html msg
selectHorizon model message =
    H.select
        [ HA.class "btn btn-outline-dark btn-sm"
        , HE.targetValue
            |> D.andThen readHorizon
            |> D.map message
            |> HE.on "change"
        ]
        (List.map (renderhorizon model.horizon.key)
            <| "All" :: (Dict.keys horizons)
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
    if List.member key (Dict.keys horizons) then
        Just key
    else
        Nothing


horizonbtnGroup : HorizonModel v -> ( Offset -> msg ) -> ( Horizon -> msg ) -> H.Html msg
horizonbtnGroup model msgOffset msgHorizon =
    H.div
        [ HA.class "row no-gutters align-items-center" ]
        [ H.div
            [ HA.class "col-sm-auto" ]
            [ let disabled = offsetDisabledLeft model in
                buttonArrow "left" disabled msgOffset
            ]
        , H.div
            [ HA.class "col-sm-auto" ]
            [ H.text model.mindate
            ]
        , H.div
            [ HA.class "col-sm-auto" ]
            [ selectHorizon model msgHorizon
            ]
        , H.div
            [ HA.class "col-sm-auto" ]
            [ H.text model.maxdate
            ]
        , H.div
            [ HA.class "col-sm-auto" ]
            [ let disabled = offsetDisabledRight model in
                buttonArrow "right" disabled msgOffset
            ]
        ]