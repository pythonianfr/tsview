port module Horizon exposing
    ( HorizonModel
    , Msg(..)
    , FrameMove(..)
    , OptionType(..)
    , FetchTrigger(..)
    , LocalStorageData
    , PlotStatus(..)
    , initHorizon
    , horizonview
    , getFromToDates
    , getFetchBounds
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
    )

import Date
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import List.Extra as List
import Util as U
import Url.Builder as UB
import Task

port saveToLocalStorage : LocalStorageData -> Cmd msg
port loadFromLocalStorage : (String -> msg) -> Sub msg


type Msg =
     FromLocalStorage String
     | Frame FrameMove
     | Fetch FetchTrigger
     | Internal InternalOperation


type FrameMove =
    HorizonSelected (Maybe String)
    | Slide Int
    | EditDateValidate


type OptionType =
    TimeZoneSelected String
    | InferredFreq Bool
    | ViewNoCache


type FetchTrigger =
     GotBounds ( Result Http.Error String)
     | GetDirectData ( Maybe ( String, String ))
     | Option OptionType


type InternalOperation =
    ToggleEdit
    | Edit FromOrTo String
    | DateNow Date.Date When
    | GotChoices ( Result Http.Error String)


type When =
    Start
    | Reset


type alias Bounds =
    { from: String
    , to: String
    , dateRef: String
    }

type Horizon =
    All
    | Disabled
    | Label String


type alias HorizonModel =
    { baseUrl: String
    , horizon : Horizon
    , dateRef: String
    , inferredFreq : Bool
    , timeZone : String
    , hasCache: Bool
    , viewNoCache: Bool
    , horizonChoices: List String
    , plotStatus : PlotStatus
    , horizonBounds: Maybe (String, String)
    , dataBounds: Maybe (String, String)
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

initHorizon: String -> String -> String -> String -> PlotStatus -> HorizonModel
initHorizon baseUrl min max debug status =
    { baseUrl = baseUrl
    , horizon = All
    , dateRef = "yyyy-mm-dd"
    , inferredFreq = False
    , timeZone = "UTC"
    , hasCache = True
    , viewNoCache = False
    , horizonChoices = []
    , plotStatus = status
    , horizonBounds = Nothing
    , dataBounds = Nothing
    , queryBounds = buildBounds min max
    , zoomBounds = Nothing
    , editBounds = False
    , editedBounds = { from = Nothing, to = Nothing }
    , debug = debug == "1"
    }


type alias LocalStorageData =
    { horizon : Maybe String
    , timeZone : String
    , inferredFreq : Bool
    }

type alias EnrichFromStorage =
    { horizon: Horizon
    , timeZone: String
    , inferredFreq: Bool
    }

toHorizonType: LocalStorageData -> EnrichFromStorage
toHorizonType fromStorage =
    { horizon = case fromStorage.horizon of
                    Nothing -> All
                    Just horizon -> Label horizon
    , timeZone = fromStorage.timeZone
    , inferredFreq = fromStorage.inferredFreq
    }

toLocalFormat: EnrichFromStorage -> LocalStorageData
toLocalFormat toStorage =
    { horizon = case toStorage.horizon of
                    All -> Nothing
                    Disabled -> Nothing
                    Label horizon -> Just horizon
    , timeZone = toStorage.timeZone
    , inferredFreq = toStorage.inferredFreq
    }


localstoragedecoder : D.Decoder LocalStorageData
localstoragedecoder =
    D.map3 LocalStorageData
        (D.field "horizon" (D.nullable D.string))
        (D.field "timeZone" D.string)
        (D.field "inferredFreq" D.bool)


getFetchBounds: HorizonModel -> (String, String)
getFetchBounds model =
    case model.queryBounds of
        Just (from, to) -> (from, to)
        Nothing -> case model.horizonBounds of
            Just (from, to) -> (from, to)
            Nothing -> ("", "")


getFromToDates: HorizonModel  -> Maybe ( String, String)
getFromToDates model =
    case model.zoomBounds of
         Just ( min, max ) ->  Just ( min, max )
         Nothing ->  case model.queryBounds of
                        Just ( min, max ) ->  Just ( min, max )
                        Nothing ->  case model.horizonBounds of
                                        Just ( min, max ) -> Just ( min, max )
                                        Nothing -> Nothing



viewdateT: HorizonModel -> Maybe ( String, String, Bool )
viewdateT model =
    let bounds = case model.zoomBounds of
                    Just ( min, max ) ->  Just ( min, max , True )
                    Nothing ->  case model.queryBounds of
                        Just ( min, max ) ->  Just ( min, max , True )
                        Nothing ->  case model.horizonBounds of
                                Just ( min, max ) -> Just ( min, max , False )
                                Nothing -> case model.dataBounds of
                                    Just ( min, max ) -> Just ( min, max , False )
                                    Nothing -> Nothing
    in
    case bounds of
        Nothing -> Nothing
        Just ( min, max , fromZoom) ->
            Just ( U.dateof min, U.dateof max, fromZoom )

viewdate: HorizonModel -> ( String, String, Bool )
viewdate model =
    let result = viewdateT model
    in
    case result of
        Nothing -> ( "yyyy-mm-dd", "yyyy-mm-dd",  False )
        Just (from, to, fromZoom ) -> (cropDate from, cropDate to, fromZoom)

setStatusPlot: HorizonModel -> PlotStatus ->HorizonModel
setStatusPlot model status =
    { model | plotStatus = status}


getBounds: HorizonModel -> ( Msg -> msg ) -> Int -> Cmd msg
getBounds model convertMsg step =
    case model.horizon of
        Disabled -> Cmd.none
        All -> Task.succeed (convertMsg ( Fetch ( GetDirectData Nothing )))
                    |> Task.perform identity
        Label horizon ->
            Http.get
                { expect = Http.expectString (\ s -> convertMsg (Fetch ( GotBounds s )))
                , url = UB.crossOrigin model.baseUrl
                      [ "new-dates"
                      , horizon
                      , model.dateRef
                      , ( String.fromInt step ) ]
                      []
                }

slideBounds: HorizonModel -> ( Msg -> msg ) -> Int -> Cmd msg
slideBounds model convertMsg step =
    let ( from, to ) = getFetchBounds model
    in
    Http.get
        { expect = Http.expectString (\ s -> convertMsg (Fetch ( GotBounds s )))
        , url = UB.crossOrigin model.baseUrl
              [ "translate-dates"
              , from
              , to
              , model.dateRef
              , ( String.fromInt step ) ]
              []
        }


getChoices: HorizonModel -> ( Msg -> msg ) -> Cmd msg
getChoices model convertMsg =
    Http.get
        { expect = Http.expectString (\ s -> convertMsg (Internal ( GotChoices s )))
        , url = UB.crossOrigin model.baseUrl
              [ "horizon-choices" ]
              []
        }

decodeBounds: D.Decoder Bounds
decodeBounds =
    D.map3 Bounds
        (D.field "from" D.string)
        (D.field "to" D.string)
        (D.field "ref-date" D.string)

decodeChoices: D.Decoder ( List String )
decodeChoices =
    D.list D.string

updateHorizon : Msg -> ( Msg -> msg ) -> HorizonModel -> ( HorizonModel, Cmd msg )
updateHorizon msg convertMsg model =
    case msg of
        FromLocalStorage rawdata ->
            case D.decodeString localstoragedecoder rawdata of
                Ok datadict ->
                    let enrichdatadict = toHorizonType datadict
                    in
                    let newdatadict  = case model.queryBounds of
                            Nothing ->  enrichdatadict
                            Just _ -> { enrichdatadict | horizon = Disabled }
                    in
                    let
                        newmodel = updatefromlocalstorage
                                        newdatadict
                                        model
                    in
                    ( newmodel
                     , case newmodel.queryBounds of
                         Nothing -> Task.perform
                                        (\ t -> convertMsg (Internal ( DateNow t Start )))  Date.today
                         Just bounds -> Cmd.batch [
                                    getChoices model convertMsg
                                    , Task.perform
                                        (\ t -> convertMsg ( Internal ( DateNow t Reset )))  Date.today
                                    , Task.perform
                                        identity
                                        (Task.succeed
                                            (convertMsg ( Fetch ( GetDirectData ( Just bounds )))))
                                    ]
                    )
                Err _ ->
                    ( model
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
                ( updatedModel
                , Cmd.batch [
                    saveToLocalStorage userprefs
                    , getBounds updatedModel convertMsg 0
                    ]
                )

            Slide i ->
                ( frameModel
                , slideBounds model convertMsg i )

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
                                            , horizon = Disabled
                                }
                  in
                    ( newmodel
                    , Task.succeed (convertMsg ( Fetch ( GetDirectData newmodel.queryBounds )))
                        |> Task.perform identity )




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

            GotChoices (Ok raw) ->
                case D.decodeString decodeChoices raw of
                    Err _ -> ({ model | plotStatus = Failure }
                             , Cmd.none)
                    Ok choices -> ({ model | horizonChoices = choices }
                                  , Cmd.none)

            GotChoices (Err emsg) ->
                ({ model | plotStatus = Failure }
                , Cmd.none)

            DateNow date when ->
                let newmodel = { model | dateRef = Date.toIsoString date }
                in
                ( newmodel
                , case when of
                    Start ->
                        Cmd.batch [
                            getBounds
                                newmodel
                                convertMsg
                                0
                            , getChoices
                                newmodel
                                convertMsg
                            ]
                    Reset -> Cmd.none
                )

        Fetch fetch ->
            case fetch of
            GotBounds (Ok raw) ->
                case D.decodeString decodeBounds raw of
                    Err _ -> ({ model | plotStatus = Failure }
                            , Cmd.none)

                    Ok bounds -> ({ model | dateRef = bounds.dateRef
                                          , horizonBounds = Just ( bounds.from
                                                                  , bounds.to )
                                   }
                               , Cmd.none )
            GotBounds (Err emsg) ->
                ({ model | plotStatus = Failure }
                , Cmd.none)

            GetDirectData queryBounds ->
                let allModel = { model | queryBounds = queryBounds
                                       , zoomBounds = Nothing
                                       , horizonBounds = Nothing
                                       , plotStatus = Loading }
                in
                    ( allModel
                    , Task.perform (\ t -> convertMsg (Internal ( DateNow t Reset ))) Date.today
                    )

            Option op ->
                let dataModel = { model |  plotStatus = Loading }
                in
                case op of
                TimeZoneSelected timeZone ->
                    let
                        newmodel =
                            { dataModel | timeZone = timeZone }
                        userprefs =
                            EnrichFromStorage
                                dataModel.horizon
                                timeZone
                                dataModel.inferredFreq

                    in
                    ( newmodel
                    , saveToLocalStorage ( toLocalFormat userprefs )
                    )

                InferredFreq isChecked ->
                    let
                        newmodel =
                            { dataModel | inferredFreq = isChecked }
                        userprefs =
                            EnrichFromStorage
                                model.horizon
                                model.timeZone
                                isChecked
                    in
                    ( newmodel
                    , saveToLocalStorage ( toLocalFormat userprefs ) )

                ViewNoCache ->
                    let
                        newmodel = { dataModel
                                   | viewNoCache = not model.viewNoCache }
                    in
                    ( newmodel
                    , Cmd.none )


cropDate: String -> String
cropDate date =
    String.left 10 date

updateInternalHorizon : Maybe String -> HorizonModel -> HorizonModel
updateInternalHorizon horizon model =
    { model
        | horizon = case horizon of
                        Nothing -> All
                        Just hor -> Label hor
    }


updatefromlocalstorage : EnrichFromStorage -> HorizonModel -> HorizonModel
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
        | dataBounds = if min /= ""
                            then Just ( min, max )
                            else Nothing
        , plotStatus = Success
    }


extendHorizonFromData : HorizonModel -> Dict String v -> HorizonModel
extendHorizonFromData model val =
    if List.length ( Dict.keys val ) == 0
    then model
    else
        case model.dataBounds of
            Nothing -> updateHorizonFromData model val
            Just ( minDate, maxDate ) -> let tsBounds = formatBoundDates val
                in
                { model
                    | dataBounds = Just ( min
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


buttonArrow : (Msg -> msg) -> Bool -> Int -> String -> H.Html msg
buttonArrow convertmsg disabled step className =
   let direction = if step <= 0 then "left" else "right"
   in
    H.button
        [ HA.class className
        , HA.disabled disabled
        , HE.onClick ( convertmsg ( Frame (Slide step)))]
        [ H.i
            [ HA.class <| String.replace "{arrow}" direction "bi bi-arrow-{arrow}" ]
            [ ]
        ]

readHorizon : HorizonModel -> String -> D.Decoder (Maybe String)
readHorizon model key =
    D.succeed <|
        if List.member key model.horizonChoices then
            Just key
        else
            Nothing


selectHorizon : HorizonModel -> (Msg -> msg) -> H.Html msg
selectHorizon model convertmsg =
    H.select
        [
        HE.targetValue
            |> D.andThen ( readHorizon model )
            |> D.map ( \mb -> convertmsg ( Frame ( HorizonSelected mb )) )
            |> HE.on "change"
        ]
        (List.map
            (renderhorizon model.horizon)
            ( ["All"]
               ++ model.horizonChoices
               ++ ( case model.horizon of
                        Disabled -> [""]
                        _ -> []
                  )
            )
        )


renderhorizon : Horizon -> String -> H.Html msg
renderhorizon selectedhorizon horizon =
    H.option
        [ HA.value horizon
        , case selectedhorizon of
            All -> HA.selected ( horizon == "All" )
            Disabled ->  HA.selected ( horizon == "" )
            Label lab ->  HA.selected ( horizon == lab )
        ]
        [ H.text horizon ]


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
            , HE.onCheck ( \ b ->  convertmsg (Fetch (Option ( InferredFreq b ))))
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
            D.succeed (convertmsg (Fetch (Option (TimeZoneSelected timeZone))))

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
            , HE.onClick ( convertmsg (Fetch ( Option ViewNoCache )))
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
            , H.li [] [ H.text ( showBounds model.dataBounds "data : " )]
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
                ( model.horizon == All )
                -1
                "btn btn-outline-dark btn-sm" ]
        , if not model.editBounds
          then H.div [ HA.class "horizon-trinity read"]
                  [ H.div
                    [ HA.class ( "widget-date" ++ classZoom )
                    , HE.onClick ( convertmsg (Internal ToggleEdit ))]
                    [ H.text min ]
                , H.div
                    []
                    [ selectHorizon model convertmsg]
                , H.div
                    [ HA.class ( "widget-date" ++ classZoom )
                    , HE.onClick ( convertmsg ( Internal ToggleEdit ))]
                    [ H.text max ]
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
                ( model.horizon == All )
                1
                "btn btn-outline-dark btn-sm" ]
        , H.div
            []
            [ inferredfreqswitch model convertmsg  ]
        , H.div
            []
            [ cacheswitch model convertmsg  ]
        , debugInfo model
        ]
