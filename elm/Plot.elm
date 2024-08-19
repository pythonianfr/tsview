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
    , defaultLayoutOptions
    , defaultoptions
    , getdata
    , plotargs
    , scatterplot
    , seriesdecoder
    )
import Horizon as ModuleHorizon
import Horizon exposing
    ( HorizonModel
    , PlotStatus(..)
    , initHorizon
    , horizonview
    , horizons
    , getFromToDates
    , loadFromLocalStorage
    , updateHorizon
    , extendHorizonFromData
    , setStatusPlot )
import Maybe.Extra as Maybe
import OrderedDict as OD
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
    , horizon : HorizonModel
    , selecting : Bool
    , loadedseries : Dict String SeriesAndInfos
    , errors : List String
    }

type alias SeriesAndInfos =
    { series: Series
    , cache: Bool
    , status : PlotStatus
    }

failedinfo = { series = Dict.empty
             , cache = False
             , status = Failure}

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
    | Horizon ModuleHorizon.Msg


convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg


getFromHorizon: Model -> String
getFromHorizon model =
    Maybe.unwrap "" (always model.horizon.mindate) model.horizon.horizon


getToHorizon: Model -> String
getToHorizon model =
    Maybe.unwrap "" (always model.horizon.maxdate) model.horizon.horizon

findmissing model =
    let
        selected = model.search.selected
        ismissing series =
            not <| Dict.member series model.loadedseries
    in List.filter ismissing selected


fetchseries: Model -> Bool -> List ( Cmd Msg )
fetchseries model reload =
    List.map
        (\name -> getdata
             { baseurl = model.baseurl
             , name = name
             , idate = Nothing
             , callback = GotPlotData name
             , nocache = (U.bool2int model.horizon.viewNoCache)
             , fromdate = getFromHorizon model
             , todate = getToHorizon model
             , horizon = model.horizon.horizon
                            |> Maybe.andThen
                                (\key-> OD.get key horizons)
                                    |> Maybe.map
                          (String.replace "{offset}"
                                (String.fromInt model.horizon.offset))
             , tzone = model.horizon.timeZone
             , inferredFreq = model.horizon.inferredFreq
             , keepnans = False
             , apipoint = "state"
             }
        )
        ( if not reload
            then findmissing model
            else model.search.selected )


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

                updatedloadedseries = Dict.insert
                                        x
                                        { series = Dict.empty
                                        , cache = False
                                        , status = Loading }
                                        model.loadedseries

            in
            ( { newmodel | loadedseries = updatedloadedseries}
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
                        loaded =
                            Dict.insert
                                name
                                { series = val
                                 , status = Success
                                 , cache = False
                                 }
                                model.loadedseries

                        horizonmodel = extendHorizonFromData model.horizon val
                        newmodel =
                            { model
                                | loadedseries = loaded
                                , horizon =  { horizonmodel | plotStatus = multiStatus loaded }}

                    in
                    U.nocmd newmodel

                Err err ->
                    doerr "gotplotdata decode" <| Decode.errorToString err

        GotPlotData name (Err err) ->
            let newmodel = U.adderror model ("gotplotdata error" ++ " -> " ++ name)
                updatedinfos = Dict.insert
                                name
                                failedinfo
                                newmodel.loadedseries
                horizonmodel = model.horizon
            in ( { newmodel | horizon =
                                { horizonmodel | plotStatus = multiStatus
                                                                updatedinfos }}
               , Cmd.none )

        -- horizon

        Horizon hMsg ->
            let ( newModelHorizon, commands ) =  updateHorizon
                                                    ( actionsHorizon model )
                                                    hMsg
                                                    model.horizon
            in
            ( { model | horizon = newModelHorizon
                      , loadedseries = resetSeries model.loadedseries}
            , commands )


actionsHorizon : Model -> HorizonModel -> List (Cmd Msg)
actionsHorizon model horizonModel =
    let
        newModel = { model | horizon = horizonModel }
    in
        fetchseries newModel True


resetSeries: Dict String SeriesAndInfos -> Dict String SeriesAndInfos
resetSeries loaded =
    Dict.fromList
        ( List.map
            (\ name -> (name,  { series = Dict.empty
                                , cache = readCache name loaded
                                , status = Loading }))
            ( Dict.keys loaded ) )


readCache: String -> Dict String SeriesAndInfos -> Bool
readCache name loaded =
    case ( Dict.get name loaded ) of
        Just info -> info.cache
        Nothing -> False


multiStatus: Dict String SeriesAndInfos -> PlotStatus
multiStatus infos =
    let status = List.map
                    (\ elt -> (Tuple.second elt).status)
                    ( Dict.toList infos )
    in
    if List.any (\ elt -> elt == Failure ) status then Failure
    else
    if List.all (\ elt -> elt == Success ) status then Success
    else
    Loading


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
            ]
            [ H.text <| "info" ]
        , H.text " "
        , H.a
            [ HA.href <| UB.relative [ "tshistory", seriesName ] []
            ]
            [ H.text <| "history" ]
        , H.text " "
        , if haseditor then
              H.a [ HA.href <| UB.relative [ "tseditor/?name=" ++ seriesName ] []
                  ]
                  [ H.text <| "editor" ]
          else
              H.text ""
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
                             let series = case Dict.get name model.loadedseries of
                                            Nothing -> Dict.empty
                                            Just infos -> infos.series
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
            plotargs plotDiv data defaultLayoutOptions

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
                [ H.span [ HA.class "action-container" ]
                    [ H.h1 [ HA.class "page-title" ] [ H.text "Quick view" ]
                    , horizonview model.horizon convertMsg "action-center" True ]
                , H.div
                    [ HA.class "quickview" ]
                    [ H.header [ ] [ selector ]
                    , H.div [ HA.id plotDiv ] []
                    , plotFigure [ HA.attribute "args" args ] []
                    , H.footer [] [ urls ]
                    ]
                ]]


sub: Model -> Sub Msg
sub model =
    -- this is a cheap (cadenced) debouncer for the search ui
    if model.selecting then
    Sub.batch [ Time.every 1000 (always MakeSearch)
              , loadFromLocalStorage
                    (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))
              ]

    else loadFromLocalStorage
            (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))


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
                    , horizon = initHorizon "" ""
                    , catalog= Catalog.empty
                    , haseditor = flags.haseditor
                    , search = (SeriesSelector.new [] "" [] selected [] [])
                    , selecting = (List.isEmpty selected)
                    , loadedseries = Dict.empty
                    , errors = []
                    }

            in ( model
               , Cmd.batch <| [
                      Cmd.map
                          GotCatalog
                          (Catalog.get model.baseurl "series" 1 Catalog.ReceivedSeries)
                     ] ++ fetchseries model False
               )

    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
