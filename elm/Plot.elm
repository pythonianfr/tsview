port module Plot exposing (main)

import Browser

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
    , defaultConfigOptions
    , defaultoptions
    , getdata
    , serializedPlotArgs
    , scatterplot
    , seriesdecoder
    )
import Horizon as ModuleHorizon
import Horizon exposing
    ( HorizonModel
    , PlotStatus(..)
    , ZoomFromPlotly
    , initHorizon
    , horizonview
    , getFromToDates
    , getFetchBounds
    , loadFromLocalStorage
    , updateHorizon
    , extendHorizonFromData
    , extractXaxis
    , extractYaxis
    , extractZoomDates
    , setStatusPlot )
import Maybe.Extra as Maybe
import OrderedDict as OD
import SeriesSelector
import Task exposing (Task)
import Time exposing (Month(..))
import Url.Builder as UB
import Util as U

port zoomPlot : ( ZoomFromPlotly -> msg ) -> Sub msg

port panActive : (Bool -> msg) -> Sub msg


type alias Model =
    { baseurl : String
    , catalog: Catalog.Model
    , haseditor : Bool
    , search : SeriesSelector.Model
    , horizon : HorizonModel
    , selecting : Bool
    , loadedseries : Dict String SeriesAndInfos
    , errors : List String
    , panActive: Bool
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
    | FromZoom ZoomFromPlotly
    | NewDragMode Bool


convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg

findmissing model =
    let
        selected = model.search.selected
        ismissing series =
            not <| Dict.member series model.loadedseries
    in List.filter ismissing selected


fetchseries: Model -> Bool -> Cmd Msg
fetchseries model reload =
    let ( start, end ) = getFetchBounds model.horizon
    in
    Cmd.batch
    ( List.map
        (\name -> getdata
             { baseurl = model.baseurl
             , name = name
             , idate = Nothing
             , callback = GotPlotData name
             , nocache = (U.bool2int model.horizon.viewNoCache)
             , fromdate = start
             , todate = end
             , horizon = Nothing
             , tzone = model.horizon.timeZone
             , inferredFreq = model.horizon.inferredFreq
             , keepnans = False
             , apipoint = "state"
             }
        )
        ( if not reload
            then findmissing model
            else model.search.selected )
    )

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
                updatedloadedseries = if ( List.member
                                             x
                                             ( Dict.keys model.loadedseries ) )
                                      then Dict.remove x  model.loadedseries
                                      else
                                           Dict.insert
                                               x
                                               { series = Dict.empty
                                               , cache = False
                                               , status = Loading }
                                               model.loadedseries
                horizonmodel = model.horizon
            in
            ( { newmodel | loadedseries = updatedloadedseries
                         , horizon =
                         { horizonmodel | plotStatus = multiStatus
                                                            updatedloadedseries }
              }
            , fetchseries newmodel False
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
                                                    hMsg
                                                    convertMsg
                                                    model.horizon
            in
            let resetModel = { model | horizon = newModelHorizon
                                     , loadedseries = resetSeries model.loadedseries}
                default = ( { model | horizon = newModelHorizon} , commands )
            in
            case hMsg of
                ModuleHorizon.Internal _ -> default
                ModuleHorizon.Frame _ -> ( resetModel
                                         , commands )
                ModuleHorizon.FromLocalStorage _ -> ( resetModel
                                                    , Cmd.batch ([ commands]))
                                                                 --, fetchseries resetModel True ]))
                ModuleHorizon.Fetch _ -> ( resetModel
                        , Cmd.batch ([ commands
                                     , fetchseries resetModel True ]))


        -- zoom
        FromZoom zoom ->
            let
                zoomDates = ( extractZoomDates zoom).x
                zoomY = ( extractZoomDates zoom).y
                horizonmodel = model.horizon
            in ({ model | horizon =
                    { horizonmodel | zoomBounds = zoomDates
                                   , zoomY = zoomY
                    }
                }
               , Cmd.none )

        NewDragMode panIsActive ->
            U.nocmd { model | panActive = panIsActive }


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
        plotDiv = "plot_div"
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
                             (if model.horizon.inferredFreq then "lines+markers" else "lines")
                             { defaultoptions | showlegend = True }
                        )
                        model.search.selected
            in
            serializedPlotArgs
                plotDiv
                data
                { defaultLayoutOptions | xaxis = extractXaxis
                                                    model.horizon.zoomBounds
                                        , yaxis = extractYaxis
                                                    model.horizon.zoomY
                                        , height = Just 700
                                        , dragMode = Just ( if model.panActive
                                                             then "pan"
                                                             else "zoom" )
                }
                defaultConfigOptions

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
                links =
                    List.map
                        (viewlinks model.haseditor)
                        model.search.selected

            in
                H.ul
                    [ ]
                    ( List.map
                        (\x -> H.li [ ] [ x ])
                        (permalink model :: links) )
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


permalink: Model -> H.Html Msg
permalink model =
    let
        names = List.map
                    (\name -> UB.string "series" name)
                    model.search.selected
        addParams = case getFromToDates model.horizon of
                        Nothing -> []
                        Just ( min, max ) -> [ UB.string "startdate" min
                                             , UB.string "enddate" max]
    in
    H.a
    [ HA.href ( UB.relative
                ["tsview"]
                ( names ++ addParams ))
    ]
    [ H.text "Permalink" ]


sub: Model -> Sub Msg
sub model =
    -- this is a cheap (cadenced) debouncer for the search ui
    if model.selecting then
    Sub.batch [ Time.every 1000 (always MakeSearch)
              , realsubs
              ]

    else realsubs

realsubs =
    Sub.batch
    [ loadFromLocalStorage
        (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))
    , zoomPlot FromZoom
    , panActive NewDragMode
    ]

main : Program
       { baseurl : String
       , selected : List String
       , haseditor : Bool
       , min: String
       , max : String
       , debug: String
       } Model Msg
main =
    let
        init flags =
            let
                selected =
                    flags.selected
                model =
                    { baseurl = flags.baseurl
                    , horizon = initHorizon
                                    flags.baseurl
                                    flags.min
                                    flags.max
                                    flags.debug
                                    None
                    , catalog= Catalog.empty
                    , haseditor = flags.haseditor
                    , search = (SeriesSelector.new [] "" [] selected [] [])
                    , selecting = (List.isEmpty selected)
                    , loadedseries = Dict.empty
                    , errors = []
                    , panActive = False
                    }

            in ( model
               , Cmd.batch ([ Catalog.get
                                model.baseurl
                                "series" 1
                                (\ h -> GotCatalog (Catalog.ReceivedSeries h))
                            , fetchseries model False
                            ])
               )

    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
