port module Plot exposing (main)

import Browser
import Bytes.Encode as Encode
import Dict exposing (Dict)
import EdiTable exposing
    ( CScalarType(..)
    , CType(..)
    , CompStatus(..)
    , Component
    , Payload(..)
    , ScalarType(..)
    , Stuff(..)
    , mergeData
    )
import File.Download as Download
import Html
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Catalog
import Json.Decode as Decode
import List.Extra as List
import Metadata as M
import Plotter exposing
    ( Series
    , Group
    , Trace
    , defaultLayoutOptions
    , defaultConfigOptions
    , defaultTraceOptions
    , defaultDateAxis
    , defaultValueAxis
    , getdata
    , getgroupplotdata
    , groupdecoder
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
    , extractDates
    , extractValues
    , extractZoomDates
    , updateZoom )
import Process
import SeriesSelector
import Set exposing (Set)
import Task exposing (Task)
import Time exposing (Month(..))
import Url.Builder as UB
import Util as U


port zoomPlot : ( ZoomFromPlotly -> msg ) -> Sub msg

port panActive : (Bool -> msg) -> Sub msg

port legendStatus: (List (String, Bool) -> msg) -> Sub msg

type alias Model =
    { baseurl : String
    , catalog: Catalog.Model -- is not used, actually
    , haseditor : Bool
    , searchSeries : SeriesSelector.Model
    , searchBasket: SeriesSelector.Model
    , searchGroup: SeriesSelector.Model
    , versionControl: Int
    , horizon : HorizonModel
    , selecting : Selecting
    , loaded : Loaded
    , registry : Registry
    , highlighted: Maybe String
    , errors : List String
    , panActive: Bool
    , legendStatus : Maybe (List (String, Bool))
    , showLegend : Bool
    , activeRequests : Set String
    , query : String
    }

type Selecting =
    ModeSeries
    | ModeBasket
    | ModeGroup
    | NoMode

type DataType =
    TypeSeries
    | TypeGroup

type RequestDataType =
    SeriesRequest
    | GroupRequest


type alias Loaded =
    { series : Dict String Series
    , groups :  Dict String Group
    }

type alias Registry =
    { series : Dict String DataInfos
    , groups :  Dict String  DataInfos
    }

type alias DataInfos =
    { cache: Bool
    , status : PlotStatus
    , secondAxis: Bool
    , basket: Maybe String
    , selected : Bool
    , aborted : Bool
    , fading : Bool
    }


emptyInfo: DataInfos
emptyInfo =
    { cache = False
    , status = Loading
    , secondAxis = False
    , basket = Nothing
    , selected = False
    , aborted = False
    , fading = False
    }

failedInfo : DataInfos
failedInfo =
    { cache = False
    , status = Failure
    , secondAxis = False
    , basket = Nothing
    , selected = False
    , aborted = False
    , fading = False
    }

type alias BasketItem =
    { name : String
    , imeta : Maybe M.Metadata
    , meta : Maybe M.Metadata
    , source : String
    , kind : String
    }


basketItemDecode : Decode.Decoder BasketItem
basketItemDecode =
    Decode.map5 BasketItem
        (Decode.field "name" Decode.string)
        (Decode.field "imeta" (Decode.succeed Nothing))
        (Decode.field "meta" (Decode.succeed Nothing))
        (Decode.field "source" Decode.string)
        (Decode.field "kind" Decode.string)


type Msg
    = GotCatalog Catalog.Msg
    | GotSeriesData Int ( Maybe String ) String (Result Http.Error String)
    | GotGroupData Int String (Result Http.Error String)
    | GotBasketCatalog (Result Http.Error String)
    | GotBasket Bool String (Result Http.Error String)
    | GotQuery (Result Http.Error String)
    | ChangeSelection Selecting
    | ToggleSeries String
    | FilterSeries String
    | ToggleBasket String
    | FilterBasket String
    | ToggleGroup String
    | FilterGroup String
    | BeforeRemove DataType String DataInfos
    | Remove DataType String
    | Highlight DataType String
    | Select DataType String
    | KindChange String Bool
    | SourceChange String Bool
    -- dates
    | Horizon ModuleHorizon.Msg
    | FromZoom ZoomFromPlotly
    -- plotly params
    | NewDragMode Bool
    | Legends ( List ( String, Bool ))
    | ShowLegend Bool
    | ToggleAxis DataType DataInfos String
    | DownloadCsv



convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg


findmissing: Model -> List String
findmissing model =
    let
        selected = model.searchSeries.selected
        ismissing series =
            not <| Dict.member series model.registry.series
    in List.filter ismissing selected


findmissinggroup: Model -> List String
findmissinggroup model =
    let
        selected = model.searchGroup.selected
        ismissing series =
            not <| Dict.member series model.registry.groups
    in List.filter ismissing selected


buildRequestId: RequestDataType -> String -> Int -> String
buildRequestId requestType name versionControl =
    let
        prefix = case requestType of
            SeriesRequest -> "series"
            GroupRequest -> "group"
    in
    prefix ++ "-" ++ name ++ "-" ++ String.fromInt versionControl


fetchsingle: Model -> String -> String ->  Maybe String -> String -> Cmd Msg
fetchsingle model start end basket name  =
    let
        requestId = buildRequestId SeriesRequest name model.versionControl
    in
    getdata
         { baseurl = model.baseurl
         , name = name
         , idate = Nothing
         , callback = GotSeriesData model.versionControl basket name
         , nocache = (U.bool2int model.horizon.viewNoCache)
         , fromdate = start
         , todate = end
         , horizon = Nothing
         , tzone = model.horizon.timeZone
         , inferredFreq = model.horizon.inferredFreq
         , keepnans = False
         , apipoint = "state"
         , exclude = "right"
         , tracker = Just requestId
         }


fetchsinglegroup: Model -> String -> String ->  String -> Cmd Msg
fetchsinglegroup model start end name  =
    let
        requestId = buildRequestId GroupRequest name model.versionControl
    in
    getgroupplotdata
         { baseurl = model.baseurl
         , name = name
         , idate = Nothing
         , callback = GotGroupData model.versionControl name
         , nocache = (U.bool2int model.horizon.viewNoCache)
         , fromdate = start
         , todate = end
         , horizon = Nothing
         , tzone = model.horizon.timeZone
         , keepnans = False
         , apipoint = "state"
         , tracker = Just requestId
         }

fetchseries: Model -> Bool -> (Model, Cmd Msg)
fetchseries model reload =
    let
        ( start, end ) = getFetchBounds model.horizon
        seriesToFetch = if not reload
                         then findmissing model
                         else ( Dict.keys model.registry.series )
        requests = List.map
            ( fetchsingle model start end Nothing )
            seriesToFetch
        requestIds = List.map
            (\name -> buildRequestId SeriesRequest name model.versionControl)
            seriesToFetch
        updatedModel = { model | activeRequests =
                                    Set.union
                                        model.activeRequests
                                        (Set.fromList requestIds) }
    in
    ( updatedModel, Cmd.batch (List.reverse requests) )

fetchgroups: Model -> Bool -> (Model, Cmd Msg)
fetchgroups model reload =
    let
        ( start, end ) = getFetchBounds model.horizon
        groupsToFetch = if not reload
                         then findmissinggroup model
                         else ( Dict.keys model.registry.groups )
        requests = List.map
            ( fetchsinglegroup model start end )
            groupsToFetch
        requestIds = List.map
            (\name -> buildRequestId GroupRequest name model.versionControl)
            groupsToFetch
        updatedModel = { model | activeRequests =
                                    Set.union
                                        model.activeRequests
                                        (Set.fromList requestIds) }
    in
    ( updatedModel, Cmd.batch (List.reverse requests) )

fetchbasket : Model -> Bool -> String -> Cmd Msg
fetchbasket model remove  name =
    Http.request
        { method = "GET"
        , headers = []
        , url = UB.crossOrigin model.baseurl
              [ "api", "series", "basket" ]
              [ UB.string "name" name ]
        , body = Http.emptyBody
        , expect = Http.expectString ( GotBasket remove name )
        , timeout = Nothing
        , tracker = Nothing
        }


getQuery : String -> String -> Cmd Msg
getQuery baseUrl queryStr =
    Http.get
        { expect = Http.expectString GotQuery
        , url = UB.crossOrigin baseUrl
              [ "api", "series", "find" ]
              [ UB.string "query" queryStr ]
        }


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
                String.length xm < 3
            then
                []
            else
                List.filter (U.fragmentsmatcher xm) xs |> List.take 20
    in
    case msg of
        GotCatalog catmsg ->
            case catmsg of
            Catalog.ReceivedSeries _ ->
                let
                    newcat =
                        Catalog.update catmsg Catalog.empty
                    newsearch =
                        SeriesSelector.fromcatalog model.searchSeries newcat
                in
                U.nocmd { model
                            | searchSeries = newsearch
                        }
            Catalog.ReceivedGroups _ ->
                let
                    newcat =
                        Catalog.update catmsg Catalog.empty
                    newsearch =
                        SeriesSelector.fromcatalog model.searchGroup newcat
                in
                U.nocmd { model
                            | searchGroup = { newsearch | items = newcat.groups }
                        }

        GotBasketCatalog (Ok raw) ->
            case Decode.decodeString ( Decode.list Decode.string) raw of
                Err err ->
                    doerr "gotbasketcatalog decode" <| Decode.errorToString err
                Ok baskets ->
                    let previous = model.searchBasket
                    in
                    U.nocmd
                        { model | searchBasket =
                                    { previous | items = baskets }
                        }

        GotBasketCatalog (Err err) ->
            doerr "gotbasketcatalog network" ""

        GotBasket remove basket (Ok raw) ->
             case Decode.decodeString ( Decode.list basketItemDecode) raw of
                Err err ->
                    doerr "gotbasket decode" <| Decode.errorToString err
                Ok items ->
                    let names = List.map .name items
                        presents = Set.toList
                                        <| Set.intersect
                                            ( Set.fromList
                                                ( Dict.keys model.registry.series )
                                            )
                                            ( Set.fromList names )
                    in
                    if remove
                    then ( model
                         , Cmd.batch
                            <| List.map
                                    (\ n ->
                                        Task.perform
                                            identity
                                            ( Task.succeed ( Remove TypeSeries n ))
                                    )
                                    presents
                         )
                    else
                    let
                        previousRegistry = model.registry
                        newRegistry = Dict.fromList
                                        <| List.map
                                             (\ n -> ( n, emptyInfo ))
                                             names
                        updated = { previousRegistry |
                                                series =
                                                    Dict.union
                                                    previousRegistry.series
                                                    newRegistry
                                  }

                        ( start, end ) = getFetchBounds model.horizon
                        newModel = { model | registry = updated
                                                  }
                    in
                        ( newModel
                        , Cmd.batch
                            <| List.map
                                    (fetchsingle newModel start end ( Just basket ))
                                     ( List.reverse names )
                        )

        GotBasket remove basketName (Err err) ->
            doerr "gotbasket network" ""

        GotQuery (Ok rawdata) ->
            case Decode.decodeString (Decode.list basketItemDecode) rawdata of
                Ok items ->
                    let
                        names = List.map .name items
                        searchSeries = model.searchSeries
                        updatedSearchSeries =
                            { searchSeries | selected = names }
                        newModel = { model
                                   | searchSeries = updatedSearchSeries
                                   , selecting = ModeSeries
                                  }
                        ( modelWithRequests, fetchCmd ) = fetchseries newModel False
                    in
                    ( modelWithRequests, fetchCmd )
                Err err ->
                    doerr "gotquery decode" <| Decode.errorToString err

        GotQuery (Err err) ->
            doerr "gotquery network" ""

        -- never used in this page
        KindChange kind checked ->
            let
                newsearch =
                    SeriesSelector.updatekinds
                        model.searchSeries
                        model.catalog
                        kind
                        checked
            in
            U.nocmd { model | searchSeries = newsearch }

        -- never used in this page
        SourceChange source checked ->
            let
                newsearch =
                    SeriesSelector.updatesources
                        model.searchSeries
                        model.catalog
                        source
                        checked
            in
            U.nocmd { model | searchSeries = newsearch }

        ChangeSelection selecting ->
            U.nocmd { model | selecting = selecting }

        ToggleSeries x ->
            let
                remove = ( List.member
                             x
                             ( Dict.keys model.registry.series )
                         )
                newmodel =
                    { model
                        | searchSeries = SeriesSelector.updateselected
                                    model.searchSeries
                                    (toggleItem x model.searchSeries.selected)
                    }
            in
            if remove
                then ( newmodel
                      , Task.perform identity ( Task.succeed ( Remove TypeSeries x ) ) )
            else
            -- in this branch, the item has been added through the selector (vs basket)
            -- and we are now in series addition
            let
                updatedloadedseries = { series = Dict.insert
                                                   x
                                                   emptyInfo
                                                   model.registry.series
                                       , groups = model.registry.groups
                                       }

                horizonmodel = model.horizon
            in
            let
                ( modelWithRequests, fetchCmd ) = fetchseries newmodel False
                finalModel = { modelWithRequests | registry = updatedloadedseries
                                                 , horizon =
                                                 { horizonmodel | plotStatus = multiStatus
                                                                                updatedloadedseries
                                                 }
                             }
            in
            ( finalModel, fetchCmd )

        BeforeRemove dType name info ->
            let
                loaded = model.loaded
                registry = model.registry
                aborting = info.status == Loading
            in
            if aborting
            then ( model
                 , Task.perform
                        identity
                        ( Task.succeed ( Remove dType name )))
            else
            case dType of
                TypeSeries ->
                    let
                        newregistry = { registry | series = Dict.insert
                                                                name
                                                                { info | fading = True}
                                                                registry.series
                                   }
                    in
                    ( { model | registry = newregistry
                              , loaded = { loaded | series = Dict.remove name loaded.series}
                      }
                    , Task.perform
                        (always ( Remove dType name ))
                        (Process.sleep 500)
                    )
                TypeGroup ->
                    let
                        newregistry = { registry | groups = Dict.insert
                                                                name
                                                                { info | fading = True}
                                                                registry.groups
                                   }
                    in
                    ( { model | registry = newregistry
                              , loaded = { loaded | groups = Dict.remove name loaded.groups}
                      }
                    , Task.perform
                        (always ( Remove dType name ))
                        (Process.sleep 500)
                    )

        Remove dType name ->
            let registry = model.registry
                loaded = model.loaded
            in
            case dType of
                TypeSeries ->
                    let
                        infos = Maybe.withDefault emptyInfo
                                    <| Dict.get name model.registry.series
                        aborting = infos.status == Loading
                        newloaded = { loaded | series = Dict.remove name loaded.series }
                        newregistry = if aborting
                                        then
                                            { registry | series = Dict.insert
                                                                    name
                                                                    { infos | aborted = True }
                                                                    registry.series }
                                        else
                                            { registry | series = Dict.remove name registry.series }

                        search = model.searchSeries
                        newsearch = { search | selected = List.remove name search.selected }
                    in
                        U.nocmd { model | loaded = newloaded
                                        , registry = newregistry
                                        , searchSeries = newsearch
                                }

                TypeGroup ->
                    let
                        infos = Maybe.withDefault emptyInfo
                                    <| Dict.get name model.registry.groups
                        aborting = infos.status == Loading
                        newloaded = { loaded | groups = Dict.remove name loaded.groups }
                        newregistry = if aborting
                                        then
                                            { registry | groups = Dict.insert
                                                                    name
                                                                    { infos | aborted = True }
                                                                    registry.groups }
                                        else
                                            { registry | groups = Dict.remove name registry.groups }
                        search = model.searchGroup
                        newsearch = { search | selected = List.remove name search.selected }
                    in
                        U.nocmd { model | loaded = newloaded
                                        , registry = newregistry
                                        , searchGroup = newsearch
                                        , highlighted = Nothing
                                }

        ToggleBasket name ->
            let
                remove =  List.member name model.searchBasket.selected
                newmodel =
                    { model
                        | searchBasket = SeriesSelector.updateselected
                                    model.searchBasket
                                    (toggleItem name model.searchBasket.selected)
                    }
            in
            ( newmodel
            , fetchbasket model remove name
            )

        Highlight dtype name ->
            if name == "no-highlight"
            then U.nocmd { model | highlighted = Nothing }
            else U.nocmd { model | highlighted = Just name }


        Select dtype name ->
            case dtype of
                TypeSeries ->
                    let registry = model.registry
                        info = Maybe.withDefault
                                    emptyInfo
                                    <| Dict.get
                                          name
                                          registry.series
                        newRegistry = { registry | series =
                                                    Dict.insert
                                                        name
                                                        { info | selected = not info.selected }
                                                        registry.series
                                      }
                    in U.nocmd { model | registry = newRegistry }
                TypeGroup ->
                    let registry = model.registry
                        info = Maybe.withDefault
                                    emptyInfo
                                    <| Dict.get
                                          name
                                          registry.groups
                        newRegistry = { registry | groups =
                                                    Dict.insert
                                                        name
                                                        { info | selected = not info.selected }
                                                        registry.groups
                                      }
                    in U.nocmd { model | registry = newRegistry }


        ToggleAxis dtype infos name ->
            case dtype of
                TypeSeries ->
                    U.nocmd { model | registry =
                                        { series =
                                            Dict.insert
                                                name
                                                { infos | secondAxis = not infos.secondAxis }
                                                model.registry.series
                                        , groups = model.registry.groups
                                        }
                            }
                TypeGroup ->
                    U.nocmd { model | registry =
                                        { groups =
                                            Dict.insert
                                                name
                                                { infos | secondAxis = not infos.secondAxis }
                                                model.registry.groups
                                        , series = model.registry.series
                                        }
                            }

        DownloadCsv ->
            let
                csvData = buildCsvData model
                csvString = csvData
                    |> List.map (String.join ",")
                    |> String.join "\n"
                csvBytes = Encode.encode (Encode.string csvString)
            in
            ( model, Download.bytes "plot_data.csv" "text/csv" csvBytes )

        FilterSeries x ->
            let
                search =
                    SeriesSelector.updatesearch model.searchSeries x
                moreSearch =
                    SeriesSelector.updatefound search
                        (keywordMatch
                             search.search
                             search.items
                        )
            in
            U.nocmd { model | searchSeries = moreSearch }


        FilterBasket x ->
            let
                searchBasket =
                    SeriesSelector.updatesearch model.searchBasket x
                filtered =
                    SeriesSelector.updatefound searchBasket
                        (keywordMatch
                             searchBasket.search
                             searchBasket.items
                        )
            in
            U.nocmd { model | searchBasket = filtered }

        ToggleGroup name ->
            let
                remove = List.member
                             name
                             ( Dict.keys model.registry.groups )
                newmodel =
                    { model
                        | searchGroup = SeriesSelector.updateselected
                                    model.searchGroup
                                    (toggleItem name model.searchGroup.selected)
                    }
            in
            if remove
                then ( newmodel
                      , Task.perform identity ( Task.succeed ( Remove TypeGroup name )))
            else
            -- loading after action on the selection (vs horizon)
            let
                updatedregistry = { groups = Dict.insert
                                                   name
                                                   emptyInfo
                                                   model.registry.groups
                                , series = model.registry.series
                                }

                horizonmodel = model.horizon
            in
            let
                ( modelWithRequests, fetchCmd ) = fetchgroups newmodel False
                finalModel = { modelWithRequests | registry = updatedregistry
                                                 , horizon =
                                                 { horizonmodel | plotStatus = multiStatus
                                                                                updatedregistry }
                             }
            in
            ( finalModel, fetchCmd )


        FilterGroup x ->
            let
                search =
                    SeriesSelector.updatesearch model.searchGroup x
                moreSearch =
                    SeriesSelector.updatefound search
                        (keywordMatch
                             search.search
                             search.items
                        )
            in
            U.nocmd { model | searchGroup = moreSearch }

        -- plot

        GotSeriesData versionControl basket name (Ok rawdata) ->
            if versionControl /= model.versionControl
            then U.nocmd model
            else
            let infos = readSInfo
                            name
                            model.registry
            in
            if infos.aborted
            then
                let registry = model.registry
                in
                    U.nocmd { model |
                                registry =
                                    { registry | series =
                                                    Dict.remove
                                                    name
                                                    registry.series
                                    }
                            }
            else
            case Decode.decodeString seriesdecoder rawdata of
                Ok val ->
                    let
                        baksetSource = case basket of
                                        Just b -> Just b
                                        Nothing -> infos.basket
                        search = model.searchSeries
                        newSearch = case basket of
                                        Nothing -> search
                                        Just b -> { search |
                                                    selected = search.selected ++ [ name ]
                                                  }
                        registry =
                            { series = Dict.insert
                                name
                                { status = Success
                                 , cache = infos.cache
                                 , basket = baksetSource
                                 , secondAxis = infos.secondAxis
                                 , selected = infos.selected
                                 , aborted = infos.aborted
                                 , fading = infos.fading
                                 }
                                model.registry.series
                            , groups = model.registry.groups
                            }
                        previousLoad = model.loaded
                        loaded = { previousLoad | series =
                                                    Dict.insert
                                                        name
                                                        val
                                                        previousLoad.series
                                 }
                        horizonmodel = extendHorizonFromData model.horizon val
                        newmodel =
                            { model
                                | registry = registry
                                , loaded = loaded
                                , searchSeries = newSearch
                                , horizon =  { horizonmodel | plotStatus = multiStatus registry }}

                    in
                    U.nocmd newmodel

                Err err ->
                    doerr "gotplotdata decode" <| Decode.errorToString err

        GotSeriesData versionControl basket name (Err err) ->
            let
                newmodel = U.adderror model ("gotplotdata error" ++ " -> " ++ name)
                updatedinfos = { series =
                                    Dict.insert
                                        name
                                        failedInfo
                                        newmodel.registry.series
                                , groups = newmodel.registry.groups
                                }
                horizonmodel = model.horizon
            in ( { newmodel | horizon =
                                { horizonmodel | plotStatus = multiStatus
                                                                updatedinfos }}
               , Cmd.none )

        GotGroupData versionControl name (Ok rawdata) ->
            if versionControl /= model.versionControl
            then U.nocmd model
            else
            let infos = readGInfo
                            name
                            model.registry
            in
            if infos.aborted
            then
                let registry = model.registry
                in
                    U.nocmd
                        { model |
                            registry =
                                { registry | groups =
                                                Dict.remove
                                                name
                                                registry.groups
                                }
                        }
            else
            case Decode.decodeString groupdecoder rawdata of
                Ok val ->
                    let
                        registry =
                            { groups = Dict.insert
                                name
                                { status = Success
                                , cache = infos.cache
                                , secondAxis = infos.secondAxis
                                , basket = infos.basket
                                , selected = infos.selected
                                , aborted = infos.aborted
                                , fading = infos.fading
                                 }
                                model.registry.groups
                            , series = model.registry.series
                            }
                        loaded =
                            { groups = Dict.insert
                                name
                                val
                                model.loaded.groups
                            , series = model.loaded.series
                            }

                        fisrtScenario = Maybe.withDefault
                                            Dict.empty
                                            ( List.head ( Dict.values val ))
                        horizonmodel = extendHorizonFromData model.horizon fisrtScenario
                        newmodel =
                            { model
                                | registry = registry
                                , loaded = loaded
                                , horizon =  { horizonmodel | plotStatus = multiStatus registry }}

                    in
                    U.nocmd newmodel

                Err err ->
                    doerr "gotgroupdata decode" <| Decode.errorToString err

        GotGroupData versionControl name (Err err) ->
            let
                newmodel = U.adderror
                            model
                            ("gotgroupdata error" ++ " -> " ++ name)
                registry = { groups =
                                Dict.insert
                                    name
                                    failedInfo
                                    newmodel.registry.groups
                            , series = newmodel.registry.series
                            }
                horizonmodel = model.horizon
            in ( { newmodel | registry = registry
                            , horizon =
                                { horizonmodel | plotStatus = multiStatus
                                                                registry }}
               , Cmd.none )


        -- horizon

        Horizon hMsg ->
            let ( newModelHorizon, commands ) =  updateHorizon
                                                    hMsg
                                                    convertMsg
                                                    model.horizon
                resetModel = { model | horizon = newModelHorizon
                                     , loaded = resetLoad model.loaded
                                     , registry = resetRegistry model.registry
                             }
                default = ( { model | horizon = newModelHorizon} , commands )
                versionControl = model.versionControl
            in
            case hMsg of
                ModuleHorizon.Internal _ -> default
                ModuleHorizon.Frame _ ->
                    ( resetModel
                    , commands
                    )
                ModuleHorizon.FromLocalStorage _ ->
                    ( { model | horizon = newModelHorizon }
                    , commands
                    )
                ModuleHorizon.Fetch _ ->
                    let
                        cancelCmd = Cmd.batch
                                        (List.map
                                            Http.cancel
                                            (Set.toList model.activeRequests)
                                        )
                        resetModelEmpty = { resetModel | activeRequests = Set.empty
                                                       , versionControl = versionControl + 1
                                          }
                        ( modelWithSeries, seriesCmd ) = fetchseries resetModelEmpty True
                        ( modelWithGroups, groupsCmd ) = fetchgroups modelWithSeries True
                    in
                    ( modelWithGroups
                    , Cmd.batch [ cancelCmd, commands, seriesCmd, groupsCmd ]
                    )


        -- zoom
        FromZoom zoom ->
            let
                horizonmodel = model.horizon
                updated = updateZoom
                                model.horizon
                                ( extractZoomDates zoom )
            in ({ model | horizon =
                    { horizonmodel | zoomBounds = updated.x
                                   , zoomY = updated.y
                    }
                }
               , Cmd.none )

        NewDragMode panIsActive ->
            U.nocmd { model | panActive = panIsActive }

        Legends legends ->
            U.nocmd { model | legendStatus = Just legends }

        ShowLegend show ->
            U.nocmd { model | showLegend = show }



resetLoad: Loaded -> Loaded
resetLoad loaded =
    { series =
        Dict.fromList
        <| List.map
            (\ name -> ( name, Dict.empty ))
            ( Dict.keys loaded.series )
    , groups =
        Dict.fromList
        <| List.map
            (\ name -> ( name, Dict.empty ))
            ( Dict.keys loaded.groups )
    }


resetRegistry: Registry -> Registry
resetRegistry registry =
    { series =
        Dict.fromList
        <| List.map
            (\ ( name, info) -> ( name
                                , { info| status = Loading }
                                )
            )
            ( Dict.toList registry.series )
    , groups =
         Dict.fromList
         <| List.map
            (\ (name, info) -> ( name
                               , { info| status = Loading }
                               )
            )
            ( Dict.toList registry.groups )
    }


readSInfo: String -> Registry -> DataInfos
readSInfo name registry =
    case ( Dict.get name registry.series ) of
        Just info -> info
        Nothing -> emptyInfo

readGInfo: String -> Registry -> DataInfos
readGInfo name registry =
    case ( Dict.get name registry.groups ) of
        Just info -> info
        Nothing -> emptyInfo


multiStatus: Registry -> PlotStatus
multiStatus infos =
    let statusS = List.map
                    (\ elt -> (Tuple.second elt).status)
                    ( Dict.toList infos.series )
        statusG = List.map
                (\ elt -> (Tuple.second elt).status)
                ( Dict.toList infos.groups )
        status = statusS ++ statusG
    in
    if List.any (\ elt -> elt == Failure ) status then Failure
    else
    if List.all (\ elt -> elt == Success ) status then Success
    else
    Loading


hasSecondAxis: Registry -> Bool
hasSecondAxis infos =
    let axisSeries = List.map (.secondAxis) ( Dict.values infos.series )
        axisGroup = List.map (.secondAxis) ( Dict.values infos.groups )
    in
        List.any identity ( axisSeries ++ axisGroup )


hasSelected: Registry -> Bool
hasSelected infos =
    let axisSeries = List.map (.selected) ( Dict.values infos.series )
        axisGroup = List.map (.selected) ( Dict.values infos.groups )
    in
        List.any identity ( axisSeries ++ axisGroup )


selectorConfig : SeriesSelector.SelectorConfig Msg
selectorConfig =
    { searchSelector =
        { action = Nothing
        , defaultText =
            H.text
                "Type some keywords in input bar for selecting time series"
        , toggleMsg = ToggleSeries
        }
    , actionSelector =
        { action = Nothing
        , defaultText = H.text ""
        , toggleMsg = ToggleSeries
        }
    , onInputMsg = FilterSeries
    , onKindChange = KindChange
    , onSourceChange = SourceChange
    , divAttrs = [ ]
    }

basketSelectorConfig : SeriesSelector.SelectorConfig Msg
basketSelectorConfig =
    { searchSelector =
        { action = Nothing
        , defaultText =
            H.text
                "Type some keywords in input bar for selecting basket(s)"
        , toggleMsg = ToggleBasket
        }
    , actionSelector =
        { action = Nothing
        , defaultText = H.text ""
        , toggleMsg = ToggleBasket
        }
    , onInputMsg = FilterBasket
    , onKindChange = KindChange --not used
    , onSourceChange = SourceChange --not used
    , divAttrs = [ ]
    }

groupSelectorConfig: SeriesSelector.SelectorConfig Msg
groupSelectorConfig =
    { searchSelector =
        { action = Nothing
        , defaultText =
            H.text
                "Type some keywords in input bar for selecting groups"
        , toggleMsg = ToggleGroup
        }
    , actionSelector =
        { action = Nothing
        , defaultText = H.text ""
        , toggleMsg = ToggleGroup
        }
    , onInputMsg = FilterGroup
    , onKindChange = KindChange --not used
    , onSourceChange = SourceChange --not used
    , divAttrs = [ ]
    }



visibility: Model -> String -> Bool
visibility model name =
    case model.legendStatus of
        Nothing -> True
        Just allStatus
            -> Tuple.second
                    <| Maybe.withDefault
                         ("noseries", True)
                         <| List.head
                            <| List.filter
                                (\( n,_ ) -> n == name)
                                allStatus


renderColor: DataType -> Model -> String -> Maybe { color : String }
renderColor dtype model name =
    let info = case dtype of
                TypeSeries -> Maybe.withDefault
                                emptyInfo
                                <| Dict.get
                                    name
                                    model.registry.series
                TypeGroup -> Maybe.withDefault
                                emptyInfo
                                <| Dict.get
                                    name
                                    model.registry.groups
        anySelected = hasSelected model.registry
    in
    case info.selected of
        True ->
            Nothing
        False ->
            case model.highlighted of
                Nothing -> case anySelected of
                            False -> Nothing
                            True -> Just { color = "rgba(0, 0, 0, 0.1)"}
                Just highlighted ->
                    if name == highlighted
                        then Just { color = "black"}
                        else Just { color = "rgba(0, 0, 0, 0.1)"}


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.class "quickview" ]
        [ H.div
            [ HA.class "main-content" ]
            [ H.span [ HA.class "action-container" ]
                [ H.h1 [ HA.class "page-title" ] [ H.text "Quick view" ]
                , horizonview model.horizon convertMsg "action-center" True
                , permalink model
                ]
            , H.div
                [ ]
                [ buildSelector model
                , H.div [HA.class "debug-view"]
                    (if model.horizon.debug
                     then ( debugView model )
                     else  []
                    )
                , H.div [ HA.id plotDiv ] []
                , plotFigure
                    [ HA.attribute "args" ( buildPlotArgs model )]
                    []
                , H.div
                    [ HA.class "under-the-plot" ]
                    [ seriesTable model
                    , H.br [] []
                    , downloadCsvButton model
                    ]
                ]
            ]]


plotDiv = "plot_div"

getSeries: Loaded -> String -> Series
getSeries loaded name =
    Maybe.withDefault Dict.empty
        <| Dict.get name loaded.series

getGroup: Loaded -> String -> Group
getGroup loaded name =
    Maybe.withDefault Dict.empty
        <| Dict.get name loaded.groups


buildGroupTraces: Model -> ( String , DataInfos ) -> List Trace
buildGroupTraces model ( name,  infos ) =
    List.map
        (\ ( scenario, series) ->
            scatterplot
                 scenario
                 (Dict.keys series)
                 (Dict.values series)
                 (if model.horizon.inferredFreq then "lines+markers" else "lines")
                 { defaultTraceOptions | showlegend = model.showLegend
                                        , visible = visibility model name
                                        , secondAxis = infos.secondAxis
                                        , line = renderColor TypeGroup model name
                 }
        )
        ( Dict.toList ( getGroup model.loaded name ))


buildPlotArgs: Model -> String
buildPlotArgs model =
    let
        series =
            List.map
                (\ ( name, infos ) ->
                    let points = getSeries model.loaded name
                    in
                     scatterplot
                     name
                     ( Dict.keys points )
                     ( Dict.values points )
                     (if model.horizon.inferredFreq then "lines+markers" else "lines")
                     { defaultTraceOptions | showlegend = model.showLegend
                                            , visible = visibility model name
                                            , secondAxis = infos.secondAxis
                                            , line = renderColor TypeSeries model name
                     }
                )
                ( Dict.toList model.registry.series )
        groups =
            List.concat
                <| List.map
                    ( buildGroupTraces model )
                    ( Dict.toList model.registry.groups )

        data = groups ++ series
    in
        serializedPlotArgs
            plotDiv
            data
            { defaultLayoutOptions |
                xaxis = { defaultDateAxis |
                            range = extractDates
                                       <| getFromToDates
                                            model.horizon
                         }
                , yaxis = { defaultValueAxis |
                                range = extractValues
                                    model.horizon.zoomY
                          }
                , yaxis2 = if not ( hasSecondAxis model.registry )
                    then Nothing
                    else
                        Just
                        { defaultValueAxis | overlaying = Just "y"
                                           , side = Just "right"
                        }
                , height = Just 600
                , dragMode = Just ( if model.panActive
                                     then "pan"
                                     else "zoom" )
                , margin = { t = 25
                           , b = 35
                           , l = 40
                           , r = 60
                           }
            }
            defaultConfigOptions


buildSelector: Model -> H.Html Msg
buildSelector model =
    let
        buttons =
            [ H.a
                  [ HE.onClick <| case model.selecting of
                                    ModeSeries -> ChangeSelection NoMode
                                    _ -> ChangeSelection ModeSeries
                  , HA.title "click to toggle selector"
                  , HA.class <| case model.selecting of
                                    ModeSeries -> "btn btn-warning"
                                    _ -> "btn btn-primary"

                  ]
                  [ H.text <| case model.selecting of
                                    ModeSeries -> "Hide selection"
                                    _ -> "Series selection"
                  ]
            , H.a
                  [ HE.onClick <| case model.selecting of
                                    ModeBasket -> ChangeSelection NoMode
                                    _ -> ChangeSelection ModeBasket
                  , HA.title "click to toggle selector"
                  , HA.class <| case model.selecting of
                                    ModeBasket -> "btn btn-warning"
                                    _ -> "btn btn-primary"

                  ]
                  [ H.text <| case model.selecting of
                                    ModeBasket -> "Hide selection"
                                    _ -> "Basket selection"
                  ]
            , H.a
                  [ HE.onClick <| case model.selecting of
                                    ModeGroup -> ChangeSelection NoMode
                                    _ -> ChangeSelection ModeGroup
                  , HA.title "click to toggle selector"
                  , HA.class <| case model.selecting of
                                    ModeGroup -> "btn btn-warning"
                                    _ -> "btn btn-primary"

                  ]
                  [ H.text <| case model.selecting of
                                    ModeGroup -> "Hide selection"
                                    _ -> "Group selection"
                  ]
            ]
    in
       H.header
            [HA.class "data-selector"]
            [ H.form [ ]
            ( buttons ++
                [ buttonLegend model ] ++
                ( case model.selecting of
                    ModeSeries ->
                      [ SeriesSelector.view
                               model.searchSeries
                               selectorConfig
                         ]
                    ModeBasket ->
                      [ SeriesSelector.view
                               model.searchBasket
                               basketSelectorConfig
                     ]
                    ModeGroup ->
                      [ SeriesSelector.view
                               model.searchGroup
                               groupSelectorConfig
                     ]
                    NoMode -> []
                )
            )
            ]


buttonLegend: Model -> H.Html Msg
buttonLegend model =
    H.div
       [ HA.class "show-legend-container"]
       [ H.div
        [ HA.class "custom-control custom-switch"
        , HA.class "button-legend"
        ]
        [ H.input
            [ HA.attribute "type" "checkbox"
            , HA.class "custom-control-input"
            , HA.id "showLegend"
            , HA.checked ( model.showLegend )
            , HE.onCheck ShowLegend
            ] [ ]
        , H.label
            [ HA.class "custom-control-label"
            , HA.for "showLegend"
            ]
            [ H.text "Show legend" ]
        ]
    ]


seriesTable: Model -> H.Html Msg
seriesTable model =
        H.table
            [ HA.class "under-table data-table"
            ]
            ( ( List.map
                ( rowGeneric model TypeSeries )
                ( Dict.toList model.registry.series )
                ) ++
                ( List.map
                    ( rowGeneric model TypeGroup )
                    ( Dict.toList model.registry.groups )
                )
            )


downloadCsvButton : Model -> H.Html Msg
downloadCsvButton model =
    let
        allSeriesLoaded =
            ( Dict.values model.registry.series
                |> List.all (\info -> info.status == Success))
            && not (Dict.isEmpty model.registry.series)
    in
    H.button
        [ HA.class "btn btn-primary"
        , HA.disabled (not allSeriesLoaded)
        , HE.onClick DownloadCsv
        ]
        [ H.text "Download as csv" ]


cleanDateFormat : String -> String
cleanDateFormat dateString =
    dateString
        |> String.replace "T" " "
        |> String.split "+"
        |> List.head
        |> Maybe.withDefault dateString


buildCsvData : Model -> List (List String)
buildCsvData model =
    let
        -- Convert loaded series to Components
        components =
            Dict.toList model.loaded.series
                |> List.map (\(name, series) ->
                    { name = name
                    , cType = Primary           --not used
                    , data =
                        Dict.map
                            (\_ mV -> Scalar (MFloat mV))
                            series
                    , tzaware = False           --not used
                    , status = CompLoaded       --not used
                    , editable = False          --not used
                    , scalarType = ScalFloat    --not used
                    }
                )
    in
    mergeData components
        |> List.map (List.map (\stuff ->
            case stuff of
                DateRow date -> cleanDateFormat date
                Header (name, _) -> name
                Cell entry ->
                    case entry.raw of
                        Just raw -> raw
                        Nothing -> ""
        ))


rowGeneric: Model -> DataType -> ( String,  DataInfos ) -> H.Html Msg
rowGeneric model dtype ( name, info ) =
    if info.fading
    then fadingRow info
    else
    let status = case info.status of
                        None -> "none"
                        Loading -> "loading"
                        Success -> "success"
                        Failure -> "failure"
        strType = case dtype of
                    TypeSeries -> "series"
                    TypeGroup -> "group"
        infoRoute = case dtype of
                    TypeSeries -> "tsinfo"
                    TypeGroup -> "groupinfo"
    in
    H.tr
        [ HA.id ( "remove-" ++ name )
        , HE.onMouseOver (Highlight dtype  name)
        , HE.onMouseLeave (Highlight dtype "no-highlight")
        , HA.class strType
        , HA.class <| if info.secondAxis
                                then "axis"
                                else "blend"
        , HA.class <|  if info.selected
                                then "selected"
                                else "and-blend"
        ]
        [ H.td
            [ HA.class "data-type" ]
            [ H.text <| strType
            ]
        , H.td
            [ HA.class "data-status"
            , HA.class status
            , HA.title status
            ]
            [H.text "●"]
        , H.td
            [ ]
            [ H.a
                [ HA.title infoRoute
                ,  HA.href
                    <| UB.relative [ infoRoute ] [ UB.string "name" name ]]
                [ H.text name ]
            ]
        , H.td
            []
            [ H.button
                [ HA.class "button-table-series"
                , HA.class <| if info.secondAxis
                                then "btn btn-info"
                                else "btn btn-success"
                , HE.onClick ( ToggleAxis dtype info name )
                ]
                [ H.text <| if info.secondAxis
                                then "2nd Axis"
                                else "1st Axis"
                ]
            ]
        , H.td
            []
            [ H.button
                [ HA.class "button-table-series"
                , HA.class ( "btn btn-primary " ++ if
                                info.selected
                                then "active"
                                else ""
                            )
                , HE.onClick ( Select dtype name )
                ]
                [ H.text <| if info.selected
                                then "Selected"
                                else "Select"
                ]
            ]
        , H.td
            []
            [ H.button
                [ HA.class "button-table-series"
                , HA.class <| if info.aborted
                                then "btn btn-danger"
                                else "btn btn-warning"
                , HE.onClick ( BeforeRemove dtype name info )
                ]
                [ if info.aborted
                    then H.text "Aborting"
                    else H.text "Remove"
                ]
            ]
         , H.td
            []
            [ H.text ( Maybe.withDefault "" info.basket )]
        ]


fadingRow: DataInfos -> H.Html Msg
fadingRow info =
    H.tr
        [ HA.class "fading" ]
        [ H.tr [] []
        , H.tr [] []
        , H.tr [] []
        , H.tr [] []
        , H.tr [] []
        , H.tr [] []
        , H.tr [] []
        ]


debugView: Model -> List ( H.Html Msg )
debugView model =
    let legendStuff = case model.legendStatus of
                            Nothing -> []
                            Just legends ->
                                ( List.map
                                    ( \ (l, s) -> if s
                                                  then (H.li
                                                        []
                                                        [ H.text l ]
                                                        )
                                                  else H.text ""
                                    )
                                    legends
                                )
        secondAxis = H.div
                        []
                        [ H.text ( "Has second axis : "
                                  ++ if hasSecondAxis model.registry
                                    then " True"
                                    else " False "
                                  )
                        ]
        pending = H.div
                    []
                    <| List.map
                            (\id -> H.p
                                    []
                                    [ H.text ("id : " ++ id)]
                            )
                            <| Set.toList model.activeRequests
        errors = H.div
                    []
                    <| List.map
                        (\ b -> H.text b)
                        model.errors
    in
        List.concat [ [ H.br [] []]
                    , legendStuff
                    , [ secondAxis ]
                    , [ pending ]
                    , [ errors ]
                    ]


permalink: Model -> H.Html Msg
permalink model =
    let
        series = List.map
                    (\name -> UB.string "series" name)
                    ( Dict.keys model.loaded.series )
        groups = List.map
                    (\name -> UB.string "group" name)
                    ( Dict.keys model.loaded.groups )
        axisS = List.map
            (\name -> UB.string "axis2S" name)
            ( secondAxisNames model TypeSeries )
        axisG = List.map
            (\name -> UB.string "axis2G" name)
            ( secondAxisNames model TypeGroup )
        addParams = case getFromToDates model.horizon of
                        Nothing -> []
                        Just ( min, max ) -> [ UB.string "startdate" min
                                             , UB.string "enddate" max]
    in
    H.a
    [ HA.class "permalink"
    , HA.href ( UB.relative
                ["tsview"]
                ( series ++ groups ++ axisS ++  axisG ++ addParams )
              )
    ]
    [ H.text "permalink" ]


secondAxisNames: Model -> DataType ->List String
secondAxisNames model dType =
    case dType of
        TypeSeries ->
            Dict.keys
                <| Dict.filter
                    (\ _ i -> i.secondAxis )
                    ( model.registry.series )
        TypeGroup ->
            Dict.keys
            <| Dict.filter
                    (\ _ i -> i.secondAxis )
                    ( model.registry.groups )


emptyRegistry: List String -> List String -> List String -> List String -> Registry
emptyRegistry series groups onSecondAxisS onSecondAxisG =
    { series = Dict.fromList
                <| List.concat
                    [ List.map
                        (\ name -> ( name
                                   , { emptyInfo | secondAxis = True })
                        )
                        onSecondAxisS
                    , List.map
                        (\ name -> ( name, emptyInfo ))
                        series
                    ]
    , groups = Dict.fromList
                <| List.concat
                    [ List.map
                        (\ name -> ( name
                                   , { emptyInfo | secondAxis = True })
                                   )
                        onSecondAxisG
                    , List.map
                         (\ name -> ( name, emptyInfo ))
                        groups
                    ]
    }


getBasketCatalog: String -> Cmd Msg
getBasketCatalog baseurl =
    Http.get
        { expect = Http.expectString GotBasketCatalog
        , url = UB.crossOrigin baseurl
              [ "api", "series", "baskets" ]
              [ ]
        }

initSearch selected = (SeriesSelector.new [] "" [] selected [] [])

sub: Model -> Sub Msg
sub model =
    Sub.batch
    [ loadFromLocalStorage
        (\ s-> convertMsg (ModuleHorizon.FromLocalStorage s))
    , zoomPlot FromZoom
    , panActive NewDragMode
    , legendStatus Legends
    ]

main : Program
       { baseurl : String
       , series : List String
       , groups : List String
       , baskets: List String
       , axis2S: List String
       , axis2G: List String
       , min: String
       , max : String
       , debug: String
       , query: String
       } Model Msg
main =
    let
        init flags =
            let
                series =
                    flags.series
                groups =
                    flags.groups
                baskets = flags.baskets
                fromScratch = ( List.isEmpty series )
                              && ( List.isEmpty groups )
                              && ( List.isEmpty baskets )
                axis2S = flags.axis2S
                axis2G = flags.axis2G
                model =
                    { baseurl = flags.baseurl
                    , horizon = initHorizon
                                    flags.baseurl
                                    flags.min
                                    flags.max
                                    flags.debug
                                    None
                    , catalog = Catalog.empty
                    , versionControl = 0
                    , haseditor = False
                    , searchSeries = initSearch series
                    , searchBasket = initSearch baskets
                    , searchGroup = initSearch groups
                    , selecting = if fromScratch && flags.query == ""
                                    then ModeSeries
                                    else NoMode
                    , loaded = { series = Dict.empty
                               , groups = Dict.empty }
                    , registry = emptyRegistry series groups axis2S axis2G
                    , highlighted = Nothing
                    , errors = []
                    , panActive = False
                    , legendStatus = Nothing
                    , showLegend = True
                    , activeRequests = Set.empty
                    , query = flags.query
                    }

                ( modelWithSeries, _ ) = fetchseries model False
                ( modelWithGroups, _ ) = fetchgroups modelWithSeries False
            in ( modelWithGroups
               , Cmd.batch ([ Catalog.get
                                model.baseurl
                                "series" 1
                                (\ h -> GotCatalog (Catalog.ReceivedSeries h))
                            , getBasketCatalog
                                model.baseurl
                            , Catalog.get
                                model.baseurl
                                "group" 1
                                (\ h -> GotCatalog (Catalog.ReceivedGroups h))
                            ] ++ ( List.map
                                    ( fetchbasket model False )
                                    baskets
                                 )
                              ++ ( if flags.query /= ""
                                   then [ getQuery model.baseurl flags.query ]
                                   else []
                                 )
                            )
               )

    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
