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
import List.Extra as List
import Metadata
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
    , horizon : HorizonModel
    , selecting : Selecting
    , loaded : Loaded
    , registry : Registry
    , highlighted: Maybe String
    , errors : List String
    , panActive: Bool
    , legendStatus : Maybe (List (String, Bool))
    , showLegend : Bool
    }

type Selecting =
    ModeSeries
    | ModeBasket
    | ModeGroup
    | NoMode

type DataType =
    TypeSeries
    | TypeGroup


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
    }


emptyInfo: DataInfos
emptyInfo =
    { cache = False
    , status = Loading
    , secondAxis = False
    , basket = Nothing
    , selected = False
    , aborted = False
    }

failedInfo : DataInfos
failedInfo =
    { cache = False
    , status = Failure
    , secondAxis = False
    , basket = Nothing
    , selected = False
    , aborted = False
    }

type alias BasketItem =
    { name : String
    , imeta : Maybe Metadata.StdMetadata
    , meta : Maybe Metadata.StdMetadata
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
    | GotSeriesData ( Maybe String ) String (Result Http.Error String)
    | GotGroupData String (Result Http.Error String)
    | GotBasketCatalog (Result Http.Error String)
    | GotBasket String (Result Http.Error String)
    | ChangeSelection Selecting
    | ToggleSeries String
    | FilterSeries String
    | ToggleBasket String
    | FilterBasket String
    | ToggleGroup String
    | FilterGroup String
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


fetchsingle: Model -> String -> String ->  Maybe String -> String -> Cmd Msg
fetchsingle model start end basket name  =
    getdata
         { baseurl = model.baseurl
         , name = name
         , idate = Nothing
         , callback = GotSeriesData basket name
         , nocache = (U.bool2int model.horizon.viewNoCache)
         , fromdate = start
         , todate = end
         , horizon = Nothing
         , tzone = model.horizon.timeZone
         , inferredFreq = model.horizon.inferredFreq
         , keepnans = False
         , apipoint = "state"
         }


fetchsinglegroup: Model -> String -> String ->  String -> Cmd Msg
fetchsinglegroup model start end name  =
    getgroupplotdata
         { baseurl = model.baseurl
         , name = name
         , idate = Nothing
         , callback = GotGroupData name
         , nocache = (U.bool2int model.horizon.viewNoCache)
         , fromdate = start
         , todate = end
         , horizon = Nothing
         , tzone = model.horizon.timeZone
         , keepnans = False
         , apipoint = "state"
         }

fetchseries: Model -> Bool -> Cmd Msg
fetchseries model reload =
    let ( start, end ) = getFetchBounds model.horizon
    in
    Cmd.batch
    ( List.map
        ( fetchsingle model start end Nothing )
        ( if not reload
            then findmissing model
            else ( Dict.keys model.registry.series ))
    )

fetchgroups: Model -> Bool -> Cmd Msg
fetchgroups model reload =
    let ( start, end ) = getFetchBounds model.horizon
    in
    Cmd.batch
    ( List.map
        ( fetchsinglegroup model start end )
        ( if not reload
            then findmissinggroup model
            else ( Dict.keys model.registry.groups ))
    )

fetchbasket : Model -> String -> Cmd Msg
fetchbasket model name =
    Http.get
        { expect = Http.expectString ( GotBasket name )
        , url = UB.crossOrigin model.baseurl
              [ "api", "series", "basket" ]
              [ UB.string "name" name ]
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
                        SeriesSelector.null
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

        GotBasket basket (Ok raw) ->
             case Decode.decodeString ( Decode.list basketItemDecode) raw of
                Err err ->
                    doerr "gotbasket decode" <| Decode.errorToString err
                Ok items ->
                    let names = List.map .name items
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
                    in
                        ( { model | registry = updated }
                        , Cmd.batch
                            <| List.map
                                    (fetchsingle model start end ( Just basket ))
                                    names
                        )

        GotBasket basketName (Err err) ->
            doerr "gotbasket network" ""

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
            ( { newmodel | registry = updatedloadedseries
                         , horizon =
                         { horizonmodel | plotStatus = multiStatus
                                                            updatedloadedseries
                         }
              }
            , fetchseries newmodel False
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
                                        , highlighted = Nothing
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
            if  List.member name model.searchBasket.selected
            then U.nocmd model
            else
            let
                newmodel =
                    { model
                        | searchBasket = SeriesSelector.updateselected
                                    model.searchBasket
                                    (toggleItem name model.searchBasket.selected)
                    }
            in
            ( newmodel
            , fetchbasket model name
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
            ( { newmodel | registry = updatedregistry
                         , horizon =
                         { horizonmodel | plotStatus = multiStatus
                                                            updatedregistry }
              }
            , fetchgroups newmodel False
            )


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

        GotSeriesData basket name (Ok rawdata) ->
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

        GotSeriesData basket name (Err err) ->
            let newmodel = U.adderror model ("gotplotdata error" ++ " -> " ++ name)
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

        GotGroupData name (Ok rawdata) ->
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

        GotGroupData name (Err err) ->
            let newmodel = U.adderror model ("gotgroupdata error" ++ " -> " ++ name)
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
            in
            let resetModel = { model | horizon = newModelHorizon
                                     , loaded = resetLoad model.loaded
                                     , registry = resetRegistry model.registry
                             }
                default = ( { model | horizon = newModelHorizon} , commands )
            in
            case hMsg of
                ModuleHorizon.Internal _ -> default
                ModuleHorizon.Frame _ -> ( resetModel
                                         , commands )
                ModuleHorizon.FromLocalStorage _ -> ( resetModel
                                                    , Cmd.batch
                                                        <| [ commands
                                                           , fetchseries resetModel False])
                ModuleHorizon.Fetch _ ->
                    ( resetModel
                    , Cmd.batch ([ commands
                                 , fetchseries resetModel True
                                 , fetchgroups resetModel True
                                 ]
                                )
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
            (\ name -> ( name,
                        { cache = (readSInfo name registry).cache
                        , status = Loading
                        , secondAxis = (readSInfo name registry).secondAxis
                        , basket = (readSInfo name registry).basket
                        , selected = (readSInfo name registry).selected
                        , aborted = (readSInfo name registry).aborted
                        }
                        )
            )
            ( Dict.keys registry.series )
    , groups =
         Dict.fromList
         <| List.map
            (\ name -> ( name,
                        { cache = (readGInfo name registry).cache
                        , status = Loading
                        , secondAxis = (readGInfo name registry).secondAxis
                        , basket = (readGInfo name registry).basket
                        , selected = (readGInfo name registry).selected
                        , aborted = (readGInfo name registry).aborted
                        }
                       )
            )
            ( Dict.keys registry.groups )
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
                , H.div
                    []
                    [ buttonLegend model ]
                , H.div [ HA.id plotDiv ] []
                , plotFigure
                    [ HA.attribute "args" ( buildPlotArgs model )]
                    []
                , H.div
                    [ HA.class "under-the-plot" ]
                    [ seriesTable model
                    , groupTable model
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
                                model.horizon.zoomBounds
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
                , margin = { t = 45
                           , b = 50
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
            []
            [ H.form [ ]
            ( buttons ++
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
            [ HA.class "under-table series-table"
            ]
            ( List.map
                ( rowGeneric model TypeSeries )
                ( Dict.toList model.registry.series )
            )

groupTable: Model -> H.Html Msg
groupTable model =
    if Dict.isEmpty model.registry.groups
        then H.table
            [ HA.class "under-table group-table"
            ]
            []
        else
        H.div
            []
            [ H.text "Groups"
            , H.table
                [ HA.class "under-table group-table"
                ]
                ( List.map
                    ( rowGeneric model TypeGroup )
                    ( Dict.toList model.registry.groups )
                )
            ]

rowGeneric: Model -> DataType -> ( String,  DataInfos ) -> H.Html Msg
rowGeneric model dtype ( name, info ) =
    H.tr
        [ HA.id ( "remove-" ++ name )
        , HE.onMouseOver (Highlight dtype  name)
        , HE.onMouseLeave (Highlight dtype "no-highlight")
        ]
        [ H.td
            [ ]
            [ H.a
                [ HA.title "tsinfo"
                ,  HA.href
                    <| UB.relative [ "tsinfo" ] [ UB.string "name" name ]]
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
                [ H.text "Highlight" ]
            ]
        , H.td
            []
            [ H.button
                [ HA.class "button-table-series"
                , HA.class "btn btn-warning"
                , HE.onClick ( Remove dtype name )
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
        errors = H.div
                    []
                    <| List.map
                        (\ b -> H.text b)
                        model.errors
    in
        List.concat [ [ H.br [] []]
                    , legendStuff
                    , [ secondAxis ]
                    , [ errors ]
                    ]


permalink: Model -> H.Html Msg
permalink model =
    let
        names = List.map
                    (\name -> UB.string "series" name)
                    ( Dict.keys model.loaded.series )
        axis = List.map
            (\name -> UB.string "axis2" name)
            (secondAxisNames model)
        addParams = case getFromToDates model.horizon of
                        Nothing -> []
                        Just ( min, max ) -> [ UB.string "startdate" min
                                             , UB.string "enddate" max]
    in
    H.a
    [ HA.class "permalink"
    , HA.href ( UB.relative
                ["tsview"]
                ( names ++ axis ++ addParams ))
    ]
    [ H.text "permalink" ]


secondAxisNames: Model -> List String
secondAxisNames model =
    Dict.keys
        <| Dict.filter
                (\ _ i -> i.secondAxis )
                ( model.registry.series )


emptyRegistry: List String -> Registry
emptyRegistry onSecondAxis =
    { series = Dict.fromList
                <| List.map
                    (\ name -> ( name
                               , { emptyInfo | secondAxis = True })
                               )
                    onSecondAxis
    , groups = Dict.empty
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
       , selected : List String
       , axis2: List String
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
                axis2 = flags.axis2
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
                    , searchSeries = initSearch []
                    , searchBasket = initSearch []
                    , searchGroup = initSearch []
                    , selecting = if (List.isEmpty selected )
                                    then ModeSeries
                                    else NoMode
                    , loaded = { series = Dict.empty, groups = Dict.empty }
                    , registry = emptyRegistry axis2
                    , highlighted = Nothing
                    , errors = []
                    , panActive = False
                    , legendStatus = Nothing
                    , showLegend = True
                    }

            in ( model
               , Cmd.batch ([ Catalog.get
                                model.baseurl
                                "series" 1
                                (\ h -> GotCatalog (Catalog.ReceivedSeries h))
                            , getBasketCatalog
                                model.baseurl
                            ,Catalog.get
                                model.baseurl
                                "group" 1
                                (\ h -> GotCatalog (Catalog.ReceivedGroups h))
                            ])
               )

    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
