port module Horizon exposing
    ( HorizonModel
    , Msg(..)
    , Move(..)
    , Option(..)
    , LocalStorageData
    , PlotStatus(..)
    , Offset
    , initHorizon
    , horizons
    , horizonview
    , getFromToDates
    , saveToLocalStorage
    , loadFromLocalStorage
    , localstoragedecoder
    , updatefromlocalstorage
    , updateHorizon
    , updateHorizonFromData
    , extendHorizonFromData
    , extractXaxis
    , extractZoomDates
    , setStatusPlot
    , setDisabled
    )

import Date
import Dict exposing (Dict)
import Either exposing (Either(..))
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import List.Extra as List
import Maybe.Extra as Maybe
import OrderedDict as OD
import Util as U

port saveToLocalStorage : LocalStorageData -> Cmd msg
port loadFromLocalStorage : (String -> msg) -> Sub msg


type alias Offset =
    Either Int Int


type Msg =
     FromLocalStorage String
     | DateNow Date.Date
     | Frame Move
     | Data Option
     | Internal Operation


type Move =
    HorizonSelected (Maybe String)
    | UpdateOffset Offset
    | EditDateValidate


type Option =
    TimeZoneSelected String
    | InferredFreq Bool
    | ViewNoCache


type Operation =
    ToggleEdit
    | Edit FromOrTo String


type alias HorizonModel =
    { offset : Int
    , horizon : Maybe String
    , dateRef: String
    , inferredFreq : Bool
    , timeZone : String
    , hasCache: Bool
    , viewNoCache: Bool
    , horizonChoices: OD.OrderedDict String String
    , plotStatus : PlotStatus
    , disabled: Bool
    , horizonBounds: Maybe (String, String)
    , queryBounds: Maybe (String, String)
    , zoomBounds: Maybe (String, String)
    , editBounds : Bool
    , editedBounds : { from: Maybe String
                     , to: Maybe String }
    , debug: Bool
    }

type PlotStatus
    = None
    | Loading
    | Success
    | Failure

type FromOrTo
    = From
    | To

buildBounds: String -> String -> Maybe (String, String)
buildBounds min max =
    if (min == "None") || (min == "")
        then Nothing
        else if ( max =="None" ) || ( max == "" )
            then Nothing
            else Just (min, max)

initHorizon min max status =
    { offset = 0
    , horizon = Just defaultHorizon
    , dateRef = "yyyy-mm-dd"
    , inferredFreq = False
    , timeZone = "UTC"
    , hasCache = True
    , viewNoCache = False
    , horizonChoices = horizons
    , plotStatus = status
    , disabled = False
    , horizonBounds = Nothing
    , queryBounds = buildBounds min max
    , zoomBounds = Nothing
    , editBounds = False
    , editedBounds = { from = Nothing, to = Nothing }
    , debug = False
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

getFromToDates: HorizonModel  -> Maybe ( String, String)
getFromToDates model =
    case model.zoomBounds of
         Just ( min, max ) ->  Just ( min, max )
         Nothing ->  case model.queryBounds of
                        Just ( min, max ) ->  Just ( min, max )
                        Nothing ->  case model.horizon of
                            Nothing -> Nothing
                            Just _ -> case model.horizonBounds of
                                        Just ( min, max ) -> Just ( min, max )
                                        Nothing -> Nothing


viewdate: HorizonModel -> ( String, String, Bool )
viewdate model =
    let bounds = case model.zoomBounds of
                    Just ( min, max ) ->  Just ( min, max , True )
                    Nothing ->  case model.queryBounds of
                        Just ( min, max ) ->  Just ( min, max , True )
                        Nothing ->  case model.horizonBounds of
                                Just ( min, max ) -> Just ( min, max , False )
                                Nothing -> Nothing
    in
    case bounds of
        Nothing -> ( "yyyy-mm-dd", "yyyy-mm-dd",  False )
        Just ( min, max , fromZoom) ->
            ( U.dateof min, U.dateof max, fromZoom )

setStatusPlot: HorizonModel -> PlotStatus ->HorizonModel
setStatusPlot model status =
    { model | plotStatus = status}

setDisabled: HorizonModel -> Bool ->HorizonModel
setDisabled model status =
    { model | disabled = status }



updateHorizon : Msg -> HorizonModel -> ( HorizonModel, Cmd msg )
updateHorizon msg model =
    let previousOffset = model.offset
    in
    case msg of
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
                     , Cmd.none )

                Err _ ->
                    ( model
                     , Cmd.none )
        DateNow date ->
            ({ model | dateRef = Date.toIsoString date }
            , Cmd.none )

        Frame op ->
            let frameModel = { model | queryBounds = Nothing
                                     , zoomBounds = Nothing
                                     , horizonBounds = Nothing
                                     , plotStatus = Loading}
            in
            case op of
            HorizonSelected horizon ->
                let
                    userprefs =
                        LocalStorageData
                            horizon
                            frameModel.timeZone
                            frameModel.inferredFreq

                    updatedModel = updateInternalHorizon horizon frameModel
                in
                let  newModel =  { updatedModel | disabled = False }
                in
                ( newModel
                , saveToLocalStorage userprefs
                )

            UpdateOffset (Left i) ->
                let newmodel = { frameModel | offset = (previousOffset + i )}
                in
                ( newmodel
                , Cmd.none )

            UpdateOffset (Right i) ->
                let newmodel = { frameModel | offset = (previousOffset - i) }
                in
                ( newmodel
                 , Cmd.none )

            EditDateValidate ->
                let
                    newmodel = { frameModel | queryBounds = Just ( Maybe.withDefault
                                                                ""
                                                                model.editedBounds.from
                                                            , Maybe.withDefault
                                                                ""
                                                                model.editedBounds.to
                                                            )
                                            , editBounds = False
                                            , editedBounds = { from = Nothing, to = Nothing }
                                            , horizon = Just ""
                                            , disabled = True
                                }
                  in
                    ( newmodel
                    , Cmd.none )

        Data op ->
            let dataModel = { model |  plotStatus = Loading }
            in
            case op of
            TimeZoneSelected timeZone ->
                let
                    newmodel =
                        { dataModel | timeZone = timeZone }
                    userprefs =
                        LocalStorageData
                            dataModel.horizon
                            timeZone
                            dataModel.inferredFreq

                in
                ( newmodel
                , saveToLocalStorage userprefs
                )

            InferredFreq isChecked ->
                let
                    newmodel =
                        { dataModel | inferredFreq = isChecked
                                    , horizonBounds = Nothing }
                    userprefs =
                        LocalStorageData
                            model.horizon
                            model.timeZone
                            isChecked
                in
                ( newmodel
                , saveToLocalStorage userprefs )

            ViewNoCache ->
                let
                    newmodel = { dataModel
                               | viewNoCache = not model.viewNoCache
                               , horizonBounds = Nothing }
                in
                ( newmodel
                , Cmd.none )

        Internal op ->
            case op of
            ToggleEdit ->
                let newmodel = { model | editBounds = not model.editBounds }
                in
                if newmodel.editBounds
                then
                    let ( from, to, _ ) = viewdate model
                    in
                    ({ newmodel | editedBounds = { from = Just from, to = Just to } }
                    , Cmd.none )
                else
                    ( { newmodel | editedBounds = { from= Nothing, to = Nothing }}
                    , Cmd.none )

            Edit fromOrTo value ->
                let previous  = model.editedBounds
                in
                    let newEdited = case fromOrTo of
                                        From -> { from = Just value, to = previous.to }
                                        To -> { from = previous.from, to = Just value }
                    in
                        ({ model | editedBounds = newEdited }, Cmd.none)


cropDate: String -> String
cropDate date =
    String.left 10 date

updateInternalHorizon : Maybe String -> HorizonModel -> HorizonModel
updateInternalHorizon horizon model =
    { model
        | horizon = horizon
        , offset = 0
    }


updatefromlocalstorage : LocalStorageData -> HorizonModel -> HorizonModel
updatefromlocalstorage data model =
    { model
        | horizon = data.horizon
        , inferredFreq = data.inferredFreq
        , timeZone = data.timeZone
    }


updateHorizonFromData : HorizonModel -> Dict String v -> HorizonModel
updateHorizonFromData model val =
    let
        ( min, max ) = formatBoundDates val
    in
    { model
        | horizonBounds = if min /= ""
                            then Just ( min, max )
                            else Nothing
        , plotStatus = Success
    }


extendHorizonFromData : HorizonModel -> Dict String v -> HorizonModel
extendHorizonFromData model val =
    if List.length ( Dict.keys val ) == 0
    then model
    else
        case model.horizonBounds of
            Nothing -> updateHorizonFromData model val
            Just ( minDate, maxDate ) -> let tsBounds = formatBoundDates val
                in
                { model
                    | horizonBounds = Just ( min
                                               ( Tuple.first tsBounds )
                                               minDate
                                           , max
                                               (Tuple.second tsBounds )
                                               maxDate
                                           )
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
        , HE.onClick ( convertmsg ( Frame (UpdateOffset ( arrow 1 ))))]
        [ H.i
            [ HA.class <| String.replace "{arrow}" direction "bi bi-arrow-{arrow}" ]
            [ ]
        ]


selectHorizon : HorizonModel -> (Msg -> msg) -> H.Html msg
selectHorizon model convertmsg =
    H.select
        [ HE.targetValue
            |> D.andThen readHorizon
            |> D.map ( \mb -> convertmsg ( Frame ( HorizonSelected mb )) )
            |> HE.on "change"
        ]
        (List.map
            (renderhorizon model.horizon)
            ( "All" ::
                List.filter
                    ( if model.disabled
                        then ( \ _ -> True)
                        else ( \ name -> name /= "" ))
                    (OD.keys horizons) )
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


extractXaxis : Maybe ( String,  String ) -> Maybe { range: List String }
extractXaxis bounds =
    case bounds of
        Nothing -> Nothing
        Just ( x0, x1 ) -> Just { range = [ x0, x1 ]}


extractZoomDates : List String -> Maybe ( String, String )
extractZoomDates dates =
    if String.join "" dates == ""
    then Nothing
    else
    let min = ( Maybe.map (String.replace " " "T") (List.head dates))
        max = ( Maybe.map (String.replace " " "T") (List.last dates))
    in
        case min of
            Nothing -> Nothing
            Just minDate -> case max of
                Nothing -> Nothing
                Just maxDate -> Just ( minDate, maxDate )

renderTimeZone : String -> String -> H.Html msg
renderTimeZone selectedhorizon timeZone =
    H.option
        [ HA.value timeZone
        , HA.selected (selectedhorizon == timeZone)
        ]
        [ H.text timeZone ]



inferredfreqswitch : HorizonModel -> (Msg -> msg) -> H.Html msg
inferredfreqswitch model convertmsg =
    H.div
        [ HA.class "custom-control custom-switch"]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "flexSwitchCheckDefault"
            , HA.checked model.inferredFreq
            , HA.disabled ( model.plotStatus == Loading )
            , HE.onCheck ( \ b ->  convertmsg (Data ( InferredFreq b )))
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "flexSwitchCheckDefault"
            ]
            [ H.text "inferred freq" ]
        ]


tzonedropdown : HorizonModel -> (Msg -> msg) -> H.Html msg
tzonedropdown model convertmsg =
    let
        decodeTimeZone : String -> D.Decoder msg
        decodeTimeZone timeZone =
            D.succeed (convertmsg (Data (TimeZoneSelected timeZone)))

    in
    H.select
        [ HE.on "change" (D.andThen decodeTimeZone HE.targetValue)
        , HA.disabled ( model.plotStatus == Loading )
        ]
        (List.map (renderTimeZone model.timeZone) ["UTC", "CET"])


loadingStatus: HorizonModel -> H.Html msg
loadingStatus model =
    H.div
        [ HA.class
            ( case model.plotStatus of
                None -> "none"
                Loading -> "loading"
                Success -> "success"
                Failure -> "failure"
            )
        ]
        [H.text "•"]

cacheswitch: HorizonModel -> (Msg -> msg) -> H.Html msg
cacheswitch model convertmsg =
    if model.hasCache
    then
    H.div
        [ HA.class "custom-control custom-switch"]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "cacheSwitch"
            , HA.checked ( not model.viewNoCache )
            , HA.disabled (model.plotStatus == Loading)
            , HE.onClick ( convertmsg ( Data ViewNoCache ))
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "cacheSwitch"
            ]
            [ H.text "cache" ]
        ]
    else H.div [] []


showBounds: Maybe (String, String) -> String -> String
showBounds bounds label =
    case bounds of
        Nothing -> label ++ "nothing"
        Just (a, b) -> label ++a ++ " - " ++ b


showEdited: { from: Maybe String, to: Maybe String} -> String -> String
showEdited edited label =
    label ++
    ( Maybe.withDefault "nothing" edited.from )
    ++ " - " ++( Maybe.withDefault "nothing" edited.to )

debugInfo: HorizonModel -> H.Html msg
debugInfo model =
    if model.debug
    then
        H.ul
            []
            [ H.li [] [ H.text ( "Date-Ref : " ++ model.dateRef )]
            , H.li [] [ H.text ( showBounds model.horizonBounds "horizon : " )]
            , H.li [] [ H.text ( showBounds model.queryBounds "query : " )]
            , H.li [] [ H.text ( showBounds model.zoomBounds "zoom : " )]
            , H.li [] [ H.text ( showEdited model.editedBounds "edited : " )]
            ]
    else
        H.div [] []

horizonview : HorizonModel -> (Msg -> msg) -> String -> Bool -> H.Html msg
horizonview model convertmsg klass tzaware =
    let ( min, max, fromZoom ) = viewdate model
    in
    let classZoom = ( if fromZoom then " from-zoom" else "" )
    in
    H.div
        [ HA.class klass ]
        [ H.div
            [ HA.class "widget-loading-status" ]
            [ loadingStatus model]
        , if tzaware
          then H.div [] [ tzonedropdown model convertmsg ]
          else H.span [] []
        , H.div
            []
            [ buttonArrow
                convertmsg
                ( model.disabled ||  model.horizon == Nothing )
                "left"
                "btn btn-outline-dark btn-sm" ]
        , if not model.editBounds
          then H.div [ HA.class "horizon-trinity read"]
                  [ H.div
                    [ HA.class ( "widget-date" ++ classZoom )
                    , HE.onClick ( convertmsg (Internal ToggleEdit ))]
                    [ H.text <| min ]
                , H.div
                    []
                    [ selectHorizon model convertmsg]
                , H.div
                    [ HA.class ( "widget-date" ++ classZoom )
                    , HE.onClick ( convertmsg ( Internal ToggleEdit ))]
                    [ H.text <| max ]
                ]

          else H.div [ HA.class "horizon-trinity write" ]
              [ H.input
                    [ HA.class ( "widget-date" ++ classZoom )
                    , HA.placeholder "yyyy-mm-dd"
                    , HA.type_ "date"
                    , HA.value ( Maybe.withDefault
                                    ""
                                    model.editedBounds.from )
                    , HE.onInput  (\ s -> convertmsg (Internal (Edit From  s )))
                    ]
                    []
                , H.div
                    [ HA.class "edit-buttons" ]
                    [ H.button
                        [ HE.onClick ( convertmsg (Frame EditDateValidate ))
                        , HA.class "yes" ]
                        [ H.text "✓" ]
                    , H.button
                        [ HE.onClick ( convertmsg (Internal ToggleEdit ))
                        , HA.class "no" ]
                        [ H.text "x" ]
                    ]
                , H.input
                    [ HA.class ( "widget-date" ++ classZoom )
                    , HA.placeholder "yyyy-mm-dd"
                    , HA.type_ "date"
                    , HA.value ( Maybe.withDefault
                                    ""
                                    model.editedBounds.to )
                    , HE.onInput (\ s -> convertmsg ( Internal (Edit To  s )))
                    ]
                    []
                ]
        , H.div
            []
            [ buttonArrow
                convertmsg
                ( model.disabled || model.horizon == Nothing )
                "right"
                "btn btn-outline-dark btn-sm" ]
        , H.div
            []
            [ inferredfreqswitch model convertmsg  ]
        , H.div
            []
            [ cacheswitch model convertmsg  ]
        , debugInfo model
        ]
