port module Horizon exposing
    ( HorizonModel
    , Msg(..)
    , LocalStorageData
    , PlotStatus(..)
    , Offset
    , initHorizon
    , horizons
    , horizonview
    , saveToLocalStorage
    , loadFromLocalStorage
    , localstoragedecoder
    , updatefromlocalstorage
    , updateHorizon
    , updateHorizonFromData
    , setStatusPlot
    , setDisabled
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


port saveToLocalStorage : LocalStorageData -> Cmd msg
port loadFromLocalStorage : (String -> msg) -> Sub msg


type alias Offset =
    Either Int Int


type Msg =
    FromLocalStorage String
    | HorizonSelected (Maybe String)
    | UpdateOffset Offset
    | TimeZoneSelected String
    | InferredFreq Bool


type alias HorizonModel v =
    { offset : Int
    , horizon : Maybe String
    , inferredFreq : Bool
    , mindate : String
    , maxdate : String
    , timeSeries : Dict String v
    , timeZone : String
    , horizonChoices: OD.OrderedDict String String
    , plotStatus : PlotStatus
    , disabled: Bool
    , queryBounds: Maybe (String, String)
    , zoomBounds: Maybe (String, String)
    }

type PlotStatus
    = None
    | Loading
    | Success
    | Failure

buildBounds: String -> String -> Maybe (String, String)
buildBounds min max =
    if (min == "None") || (min == "")
        then Nothing
        else if ( max =="None" ) || ( max == "" )
            then Nothing
            else Just (min, max)

initHorizon min max =
    { offset = 0
    , horizon = Just defaultHorizon
    , inferredFreq = False
    , mindate = ""
    , maxdate = ""
    , timeSeries = Dict.empty
    , timeZone = "UTC"
    , horizonChoices = horizons
    , plotStatus = None
    , disabled = False
    , queryBounds = buildBounds min max
    , zoomBounds = Nothing
    }


type alias LocalStorageData =
    { horizon : Maybe String
    , timeZone : String
    , inferredFreq : Bool
    }


localstoragedecoder : D.Decoder LocalStorageData
localstoragedecoder =
    D.map3 LocalStorageData
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
    , ( ""
      , """
         (horizon #:date (now)
         #:past (delta #:days -1)
         #:future (delta #:days 1)
         #:offset {offset})
         """
      )
    ]


setStatusPlot: HorizonModel v -> PlotStatus ->HorizonModel v
setStatusPlot model status =
    { model | plotStatus = status}

setDisabled: HorizonModel v -> Bool ->HorizonModel v
setDisabled model status =
    { model | disabled = status }

updateHorizon : ( HorizonModel v -> ( List ( Cmd msg ))) -> Msg -> HorizonModel v -> ( HorizonModel v, Cmd msg )
updateHorizon actions msg model =
    let previousOffset = model.offset
    in
    case msg of
        HorizonSelected horizon ->
            let
                userprefs =
                    LocalStorageData
                        horizon
                        model.timeZone
                        model.inferredFreq

                newmodel = updateInternalHorizon horizon model
            in
            let updatedModel =  { newmodel | queryBounds = Nothing
                                           , zoomBounds = Nothing
                                           , plotStatus = Loading
                                           , disabled = False }
            in
            ( updatedModel
            , Cmd.batch
                ( [ saveToLocalStorage userprefs ] ++ (actions updatedModel)
                )
            )

        UpdateOffset (Left i) ->
            let newmodel = { model | offset = (previousOffset + i)
                                   , plotStatus = Loading}
            in
            ( newmodel
            , Cmd.batch ( actions newmodel ))

        UpdateOffset (Right i) ->
            let newmodel = { model | offset = (previousOffset - i)
                                   , plotStatus = Loading}
            in
            ( newmodel
             , Cmd.batch ( actions newmodel ))

        FromLocalStorage rawdata ->
            case D.decodeString localstoragedecoder rawdata of
                Ok datadict ->
                    let (newdatadict, disabled)  = case model.queryBounds of
                            Nothing ->  ( datadict, False)
                            Just _ ->  ( { datadict | horizon = Just "" }, True )
                    in
                    let
                        newmodel =
                            setDisabled
                                ( updatefromlocalstorage
                                    newdatadict
                                    model )
                                disabled
                    in
                    ( newmodel
                     , Cmd.batch ( actions newmodel ))

                Err _ ->
                    ( model
                     , Cmd.batch ( actions model ))

        TimeZoneSelected timeZone ->
            let
                newmodel =
                    { model | timeZone = timeZone
                            , plotStatus = Loading}
                userprefs =
                    LocalStorageData
                        model.horizon
                        timeZone
                        model.inferredFreq

            in
            ( newmodel
            , Cmd.batch ([ saveToLocalStorage userprefs ] ++ ( actions newmodel ))
            --, I.getidates newmodel "series" InsertionDates  To be investigated
            )

        InferredFreq isChecked ->
            let
                newmodel =
                    { model | inferredFreq = isChecked
                            , plotStatus = Loading}

                userprefs =
                    LocalStorageData
                        model.horizon
                        model.timeZone
                        isChecked
            in
            ( newmodel
            , Cmd.batch
                [ Cmd.batch ( actions model )
                , saveToLocalStorage userprefs
                ]
            )


updateInternalHorizon : Maybe String -> HorizonModel v -> HorizonModel v
updateInternalHorizon horizon model =
    { model
        | horizon = horizon
        , offset = 0
    }


updatefromlocalstorage : LocalStorageData -> HorizonModel v -> HorizonModel v
updatefromlocalstorage data model =
    { model
        | horizon = data.horizon
        , inferredFreq = data.inferredFreq
        , timeZone = data.timeZone
    }


updateHorizonFromData : HorizonModel v -> Dict String v -> HorizonModel v
updateHorizonFromData model val =
    let
        tsBounds = formatBoundDates val
    in
    { model
        | mindate = Tuple.first tsBounds
        , maxdate = Tuple.second tsBounds
        , timeSeries = val
        , plotStatus = Success
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
    in
    ( U.dateof minappdate, U.dateof maxappdate )


buttonArrow : (Msg -> msg) -> Bool -> String  -> String -> H.Html msg
buttonArrow convertmsg disabled direction className =
    let
        arrow = if direction == "left" then Left else Right
    in
    H.button
        [ HA.class className
        , HA.disabled disabled
        , HE.onClick ( convertmsg (UpdateOffset (arrow 1)))]
        [ H.i
            [ HA.class <| String.replace "{arrow}" direction "bi bi-arrow-{arrow}" ]
            [ ]
        ]


selectHorizon : HorizonModel v -> (Msg -> msg) -> H.Html msg
selectHorizon model convertmsg =
    H.select
        [ HE.targetValue
            |> D.andThen readHorizon
            |> D.map ( \mb -> convertmsg (HorizonSelected mb) )
            |> HE.on "change"
        ]
        (List.map (renderhorizon model.horizon)
            <| "All" :: (OD.keys horizons)
        )


renderhorizon : Maybe String -> String -> H.Html msg
renderhorizon selectedhorizon horizon =
    H.option
        [ HA.value horizon
        , HA.selected <| Maybe.unwrap False ((==) horizon) selectedhorizon
        ]
        [ H.text horizon ]


readHorizon : String -> D.Decoder (Maybe String)
readHorizon key =
    D.succeed <|
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



inferredfreqswitch : HorizonModel v -> (Msg -> msg) -> H.Html msg
inferredfreqswitch model convertmsg =
    H.div
        [ HA.class "custom-control custom-switch"]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "flexSwitchCheckDefault"
            , HA.checked model.inferredFreq
            , HA.disabled model.disabled
            , HE.onCheck ( \ b ->  convertmsg (InferredFreq b) )
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "flexSwitchCheckDefault"
            ]
            [ H.text "inferred freq" ]
        ]


tzonedropdown : HorizonModel v -> (Msg -> msg) -> H.Html msg
tzonedropdown model convertmsg =
    let
        decodeTimeZone : String -> D.Decoder msg
        decodeTimeZone timeZone =
            D.succeed (convertmsg (TimeZoneSelected timeZone))

    in
    H.select
        [ HE.on "change" (D.andThen decodeTimeZone HE.targetValue)
        , HA.disabled model.disabled
        ]
        (List.map (renderTimeZone model.timeZone) ["UTC", "CET"])


viewdate strdate =
    if String.length strdate == 0
    then "yyyy-mm-dd"
    else strdate


horizonview : HorizonModel v -> (Msg -> msg) -> String -> Bool -> H.Html msg
horizonview model convertmsg klass tzaware =
    H.div
        [ HA.class klass ]
        [ if tzaware
          then H.div [] [ tzonedropdown model convertmsg ]
          else H.span [] []
        , H.div
            []
            [ buttonArrow convertmsg model.disabled "left" "btn btn-outline-dark btn-sm" ]
        , H.div
            [ HA.class "widget-date" ]
            [ H.text <| viewdate model.mindate ]
        , H.div
            []
            [ selectHorizon model convertmsg]
        , H.div
            [ HA.class "widget-date" ]
            [ H.text <| viewdate model.maxdate ]
        , H.div
            []
            [ buttonArrow convertmsg model.disabled "right" "btn btn-outline-dark btn-sm" ]
        , H.div
            []
            [ inferredfreqswitch model convertmsg  ]
        ]
