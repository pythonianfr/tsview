module Plot exposing (main)

import Browser
import Date exposing
    ( Date
    , add
    , fromIsoString
    , toIsoString
    )
import Dict exposing (Dict)
import Html
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Catalog
import Json.Decode as Decode
import KeywordSelector
import Plotter exposing
    ( Series
    , defaultoptions
    , getdata
    , plotargs
    , scatterplot
    , seriesdecoder
    )
import SeriesSelector
import Task exposing (Task)
import Time exposing (Month(..))
import Url.Builder as UB
import Util as U


type alias Model =
    { baseurl : String
    , catalog: Catalog.Model
    , haseditor : Bool
    , search : SeriesSelector.Model
    , selecting : Bool
    , loadedseries : Dict String Series
    , now : Date
    , mindate : Maybe Date
    , maxdate : Maybe Date
    , errors : List String
    }


type Msg
    = GotCatalog Catalog.Msg
    | GotPlotData String (Result Http.Error String)
    | ToggleSelection
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | KindChange String Bool
    | SourceChange String Bool
    -- dates
    | GotToday Date
    | FvdatePickerChanged String
    | TvdatePickerChanged String



zerotime = "00:00:00.000Z"


timepart rest =
    case rest of
        [ time ] -> time
        _ -> zerotime


maybedate dateresult =
    case dateresult of
        Err _ -> Nothing
        Ok d -> Just d


asmaybedate bump strdate =
    case String.split "T" strdate of
        date::time ->
            let realdate = maybedate (fromIsoString date)
            in if (timepart time) == zerotime
               then realdate
            else if bump then
                     -- if there is a time residue, we bump to the next day
                     -- to make sure the max date includes all points
                     Maybe.map (\d -> Date.add Date.Days 1 d) realdate
                 else realdate

        [] -> Nothing


serializedate date =
    case date of
        Nothing -> ""
        Just d -> toIsoString d


selectmaybedate model op date1 date2 =
    -- apply an operator (min/max) on maybe dates
    case date1 of
        Nothing ->
            case date2 of
                Nothing -> model.now
                Just d2 -> d2
        Just d1 ->
            case date2 of
                Nothing -> d1
                Just d2 ->
                    op d1 d2


fetchseries model restrict =
    let
        selected =
            model.search.selected
        ismissing series =
            not <| Dict.member series model.loadedseries
        missing =
            List.filter ismissing selected
    in
    List.map
        (\name -> getdata
             { baseurl = model.baseurl
             , name = name
             , idate = Nothing
             , callback = GotPlotData name
             , nocache = 0
             , fromdate = (if restrict then (serializedate model.mindate) else "")
             , todate = (if restrict then (serializedate model.maxdate) else "")
             , horizon = Nothing
             , tzone = "UTC"
             , inferredFreq = True
             , keepnans = False
             , apipoint = "state"
             }
        )
        missing


plotFigure : List (H.Attribute msg) -> List (H.Html msg) -> H.Html msg
plotFigure =
    H.node "plot-figure"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doerr tag error =
            U.nocmd <| U.adderror model (tag ++ " -> " ++ error)
        removeItem x xs =
            List.filter ((/=) x) xs
        toggleItem x xs =
            if
                List.member x xs
            then
                removeItem x xs
            else
                x :: xs
        keywordMatch xm xs =
            if
                String.length xm < 2
            then
                []
            else
                KeywordSelector.select xm xs |> List.take 20
    in
    case msg of
        GotCatalog catmsg ->
            let
                newcat =
                    Catalog.update catmsg model.catalog
                newsearch =
                    SeriesSelector.fromcatalog model.search newcat
            in
            U.nocmd { model
                        | catalog = newcat
                        , search = newsearch
                    }

        KindChange kind checked ->
            let
                newsearch =
                    SeriesSelector.updatekinds
                        model.search
                        model.catalog
                        kind
                        checked
            in
            U.nocmd { model | search = newsearch }

        SourceChange source checked ->
            let
                newsearch =
                    SeriesSelector.updatesources
                        model.search
                        model.catalog
                        source
                        checked
            in
            U.nocmd { model | search = newsearch }

        ToggleSelection ->
            U.nocmd { model | selecting = not model.selecting }

        ToggleItem x ->
            let
                newmodel =
                    { model
                        | search = SeriesSelector.updateselected
                          model.search
                          (toggleItem x model.search.selected)
                    }
            in
            ( newmodel
            , Cmd.batch <| fetchseries newmodel False
            )

        SearchSeries x ->
            let
                search =
                    SeriesSelector.updatesearch model.search x
            in
            U.nocmd { model | search = search }

        MakeSearch ->
            let
                search =
                    SeriesSelector.updatefound model.search
                        (keywordMatch
                             model.search.search
                             model.search.filteredseries
                        )
            in
            U.nocmd { model | search = search }

        -- plot

        GotPlotData name (Ok rawdata) ->
            case Decode.decodeString seriesdecoder rawdata of
                Ok val ->
                    let
                        dates =
                            Dict.keys val
                        mindate =
                            asmaybedate False <|
                                case dates of
                                    head::_ -> head
                                    []  -> "1900-1-1"
                        maxdate =
                            asmaybedate True
                                <| Maybe.withDefault "2100-1-1" <| List.maximum dates

                        loaded =
                            Dict.insert name val model.loadedseries

                        newmodel =
                            { model
                                | loadedseries = loaded
                                , mindate =
                                  Just <| selectmaybedate model Date.min model.mindate mindate
                                , maxdate =
                                  Just <| selectmaybedate model Date.max model.maxdate maxdate
                            }
                    in
                    U.nocmd newmodel

                Err err ->
                    doerr "gotplotdata decode" <| Decode.errorToString err

        GotPlotData name (Err err) ->
            doerr "gotplotdata error" <| U.unwraperror err

        -- dates

        GotToday now ->
            U.nocmd { model | now = now }

        FvdatePickerChanged value ->
            let
                newmodel =
                    { model
                        | mindate = maybedate <| fromIsoString value
                        , loadedseries = Dict.empty
                    }
            in
            ( newmodel
            , Cmd.batch <| fetchseries newmodel True
            )

        TvdatePickerChanged value ->
            let
                newmodel =
                    { model
                        | maxdate = maybedate <| fromIsoString value
                        , loadedseries = Dict.empty
                    }
            in
            ( newmodel
            , Cmd.batch <| fetchseries newmodel True
            )


selectorConfig : SeriesSelector.SelectorConfig Msg
selectorConfig =
    { searchSelector =
        { action = Nothing
        , defaultText =
            H.text
                "Type some keywords in input bar for selecting time series"
        , toggleMsg = ToggleItem
        }
    , actionSelector =
        { action = Nothing
        , defaultText = H.text ""
        , toggleMsg = ToggleItem
        }
    , onInputMsg = SearchSeries
    , onKindChange = KindChange
    , onSourceChange = SourceChange
    , divAttrs = [ ]
    }


viewlinks haseditor seriesName =
    H.div [ ]
        [ H.text (seriesName ++ " ")
        , H.a
            [ HA.href <| UB.relative [ "tsinfo" ] [ UB.string "name" seriesName]
            , HA.target "_blank"
            ]
            [ H.text <| "info" ]
        , H.text " "
        , H.a
            [ HA.href <| UB.relative [ "tshistory", seriesName ] []
            , HA.target "_blank"
            ]
            [ H.text <| "history" ]
        , H.text " "
        , if haseditor then
              H.a [ HA.href <| UB.relative [ "tseditor/?name=" ++ seriesName ] []
                  , HA.target "_blank"
                  ]
                  [ H.text <| "editor" ]
          else
              H.text ""
        ]


viewdatepicker model =
    H.div
        [ ]
        [ H.span
              [ ]
              [ H.text " " ]
        , H.label
            [ HA.for "fvd-picker" ]
            [ H.text "from value date" ]
        , H.span
            [ ]
            [ H.text " " ]
        , H.input
            [ HA.type_ "date"
            , HA.id "fvd-picker"
            , HA.name "fvd-picker"
            , HA.value (serializedate model.mindate)
            , HE.onInput FvdatePickerChanged
            ]
            [ ]
        , H.span
            [ ]
            [ H.text " " ]
        , H.label
            [ HA.for "tvd-picker" ]
            [ H.text "to value date" ]
        , H.span
            [ ]
            [ H.text " " ]
        , H.input
            [ HA.type_ "date"
            , HA.id "tvd-picker"
            , HA.name "tvd-picker"
            , HA.value (serializedate model.maxdate)
            , HE.onInput TvdatePickerChanged
            ]
            [ ]
        ]


view : Model -> H.Html Msg
view model =
    let
        plotDiv = "plotly_div"
        args =
            let
                data =
                    List.map
                        (\name ->
                             let series =
                                     Maybe.withDefault
                                     Dict.empty <|Dict.get name model.loadedseries
                             in
                             scatterplot
                             name
                             (Dict.keys series)
                             (Dict.values series)
                             "lines"
                             { defaultoptions | showlegend = True }
                        )
                        model.search.selected
            in
            plotargs plotDiv data ""

        selector =
            let
                children =
                    [ H.a
                          [ HE.onClick ToggleSelection
                          , HA.title "click to toggle selector"
                          , HA.class "btn btn-primary"
                          ]
                          [ H.text "Series selection" ]
                    ]
            in
                H.form [ ]
                    (
                     if
                         model.selecting
                     then
                         List.append children
                             [ SeriesSelector.view
                                   model.search
                                   model.catalog
                                   selectorConfig
                             ]
                     else
                         children
                    )

        urls =
            let
                permalink =
                    let
                        url =
                            UB.relative
                                [ "tsview" ]
                                <| List.map
                                    (\x -> UB.string "series" x)
                                    model.search.selected
                    in
                    H.a
                        [ HA.href url ]
                        [ H.text "Permalink" ]

                links =
                    List.map
                        (viewlinks model.haseditor)
                        model.search.selected

            in
                H.ul
                    [ ]
                    <| List.map
                        (\x -> H.li [ ] [ x ])
                        (permalink :: links)
    in
    H.div
        [ ]
        [ H.div
                [ HA.class "main-content" ]
                [ H.h1 [ HA.class "page-title" ] [ H.text "Quick view" ]
                , H.div
                    [ HA.class "quickview" ]
                    [ H.header [ ] [ selector ]
                    , H.div [ HA.id plotDiv ] []
                    , viewdatepicker model
                    , plotFigure [ HA.attribute "args" args ] []
                    , H.footer [] [ urls ]
                    ]
                ]]


sub: Model -> Sub Msg
sub model =
    -- this is a cheap (cadenced) debouncer for the search ui
    if model.selecting then
        Time.every 1000 (always MakeSearch)
    else Sub.none


main : Program
       { baseurl : String
       , selected : List String
       , haseditor : Bool
       } Model Msg
main =
    let
        init flags =
            let
                selected =
                    flags.selected
                model =
                    { baseurl = flags.baseurl
                    , catalog= Catalog.empty
                    , haseditor = flags.haseditor
                    , search = (SeriesSelector.new [] "" [] selected [] [])
                    , selecting = (List.isEmpty selected)
                    , loadedseries = Dict.empty
                    , now =  (Date.fromCalendarDate 1900 Jan 1)
                    , mindate = Nothing
                    , maxdate = Nothing
                    , errors = []
                    }

            in ( model
               , Cmd.batch <| [
                      Cmd.map
                          GotCatalog
                          (Catalog.get model.baseurl "series" 1 Catalog.ReceivedSeries)
                     , Date.today |> Task.perform GotToday
                     ] ++ fetchseries model False
               )

    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
