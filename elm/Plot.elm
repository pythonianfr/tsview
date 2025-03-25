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
import Metadata
import Plotter exposing
    ( Series
    , defaultLayoutOptions
    , defaultConfigOptions
    , defaultTraceOptions
    , defaultDateAxis
    , defaultValueAxis
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
    , horizon : HorizonModel
    , selecting : Selecting
    , loadedseries : Dict String SeriesAndInfos
    , highlighted: Maybe String
    , fixHighlighted : Set String
    , errors : List String
    , panActive: Bool
    , legendStatus : Maybe (List (String, Bool))
    , showLegend : Bool
    }

type Selecting =
    ModeSeries
    | ModeBasket
    | NoMode

type alias SeriesAndInfos =
    { series: Series
    , cache: Bool
    , status : PlotStatus
    , secondAxis: Bool
    , basket: Maybe String
    }

emptySeriesInfo: SeriesAndInfos
emptySeriesInfo =
    { series = Dict.empty
    , cache = False
    , status = Loading
    , secondAxis = False
    , basket = Nothing
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


failedinfo = { series = Dict.empty
             , cache = False
             , status = Failure
             , secondAxis = False
             , basket = Nothing
             }

type Msg
    = GotCatalog Catalog.Msg
    | GotPlotData ( Maybe String ) String (Result Http.Error String)
    | GotBasketCatalog (Result Http.Error String)
    | GotBasket String (Result Http.Error String)
    | ChangeSelection Selecting
    | ToggleSeries String
    | RemoveSeries String
    | FilterSeries String
    | ToggleBasket String
    | FilterBasket String
    | Highlight String
    | FixedHighlight String
    | KindChange String Bool
    | SourceChange String Bool
    -- dates
    | Horizon ModuleHorizon.Msg
    | FromZoom ZoomFromPlotly
    -- plotly params
    | NewDragMode Bool
    | Legends ( List ( String, Bool ))
    | ShowLegend Bool
    | ToggleAxis String



convertMsg : ModuleHorizon.Msg -> Msg
convertMsg msg =
    Horizon msg


findmissing: Model -> List String
findmissing model =
    let
        selected = model.searchSeries.selected
        ismissing series =
            not <| Dict.member series model.loadedseries
    in List.filter ismissing selected


fetchsingle: Model -> String -> String ->  Maybe String -> String -> Cmd Msg
fetchsingle model start end basket name  =
    getdata
         { baseurl = model.baseurl
         , name = name
         , idate = Nothing
         , callback = GotPlotData basket name
         , nocache = (U.bool2int model.horizon.viewNoCache)
         , fromdate = start
         , todate = end
         , horizon = Nothing
         , tzone = model.horizon.timeZone
         , inferredFreq = model.horizon.inferredFreq
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
            else ( Dict.keys model.loadedseries ))
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
            let
                newcat =
                    Catalog.update catmsg model.catalog
                newsearch =
                    SeriesSelector.fromcatalog model.searchSeries newcat
            in
            U.nocmd { model
                        | catalog = newcat
                        , searchSeries = newsearch
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
                        newLoaded = Dict.fromList
                                        <| List.map
                                             (\ n -> (n, emptySeriesInfo))
                                             names
                        updated = Dict.union
                                    model.loadedseries
                                    newLoaded
                        ( start, end ) = getFetchBounds model.horizon
                    in
                        ( { model | loadedseries = updated }
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
                             ( Dict.keys model.loadedseries )
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
                      , Task.perform identity ( Task.succeed ( RemoveSeries x ) ) )
            else
            -- in this branch, the item has been added through the selector (vs basket)
            -- and we are now in series addition
            let
                updatedloadedseries = Dict.insert
                                       x
                                       emptySeriesInfo
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

        RemoveSeries name ->
            let
                highlighted = Set.remove name model.fixHighlighted
                loaded = Dict.remove name model.loadedseries
            in
                U.nocmd { model | fixHighlighted = highlighted
                                , loadedseries = loaded
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

        Highlight name ->
            if name == "no-highlight"
            then U.nocmd { model | highlighted = Nothing }
            else U.nocmd { model | highlighted = Just name }


        FixedHighlight name ->
            let
                remove = Set.member name model.fixHighlighted
                previous = model.fixHighlighted
            in
            case remove of
                False -> U.nocmd { model | fixHighlighted = Set.insert name previous }
                True -> U.nocmd { model | fixHighlighted = Set.remove name previous }



        ToggleAxis name ->
            case Dict.get name model.loadedseries of
                Nothing -> U.nocmd model
                Just infos ->
                    U.nocmd { model | loadedseries =
                                        Dict.insert
                                            name
                                            { infos | secondAxis = not infos.secondAxis }
                                            model.loadedseries
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

        -- plot

        GotPlotData basket name (Ok rawdata) ->
            case Decode.decodeString seriesdecoder rawdata of
                Ok val ->
                    let
                        infos = readInfo
                                    name
                                    model.loadedseries
                        updateBasket = case infos.basket of
                                            Nothing -> basket
                                            Just bask -> Just bask
                        loaded =
                            Dict.insert
                                name
                                { series = val
                                 , status = Success
                                 , cache = False
                                 , basket = updateBasket
                                 , secondAxis = infos.secondAxis
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

        GotPlotData basket name (Err err) ->
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
                                                    , Cmd.batch
                                                        <| [ commands
                                                           , fetchseries resetModel False])
                ModuleHorizon.Fetch _ -> ( resetModel
                        , Cmd.batch ([ commands
                                     , fetchseries resetModel True ]))


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



resetSeries: Dict String SeriesAndInfos -> Dict String SeriesAndInfos
resetSeries loaded =
    Dict.fromList
        ( List.map
            (\ name -> (name,  { series = Dict.empty
                                , cache = (readInfo name loaded).cache
                                , status = Loading
                                , secondAxis = (readInfo name loaded).secondAxis
                                , basket = (readInfo name loaded).basket
                                }
                        )
            )
            ( Dict.keys loaded ) )


readInfo: String -> Dict String SeriesAndInfos -> SeriesAndInfos
readInfo name loaded =
    case ( Dict.get name loaded ) of
        Just info -> info
        Nothing -> emptySeriesInfo


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


hasSecondAxis: Dict String SeriesAndInfos -> Bool
hasSecondAxis infos =
    let axis = List.map (.secondAxis) ( Dict.values infos )
    in
        List.any identity axis


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


renderColor: Model -> String -> Maybe { color : String }
renderColor model name =
    case Set.isEmpty model.fixHighlighted of
        False ->
            if Set.member name model.fixHighlighted
                then Nothing
                else
                case model.highlighted of
                    Nothing -> Just { color = "rgba(0, 0, 0, 0.1)"}
                    Just highlighted ->
                        if name == highlighted
                            then Just { color = "black"}
                            else Just { color = "rgba(0, 0, 0, 0.1)"}
        True ->
            case model.highlighted of
                Nothing -> Nothing
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
                    [ seriesTable model ]
                ]
            ]]


plotDiv = "plot_div"


buildPlotArgs: Model -> String
buildPlotArgs model =
    let
        data =
            List.map
                (\name ->
                     let infos = case Dict.get name model.loadedseries of
                                    Nothing -> emptySeriesInfo
                                    Just info -> info
                     in
                     scatterplot
                     name
                     (Dict.keys infos.series)
                     (Dict.values infos.series)
                     (if model.horizon.inferredFreq then "lines+markers" else "lines")
                     { defaultTraceOptions | showlegend = model.showLegend
                                            , visible = visibility model name
                                            , secondAxis = infos.secondAxis
                                            , line = renderColor model name
                     }
                )
                ( List.sort ( Dict.keys model.loadedseries ))
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
                , yaxis2 = if not ( hasSecondAxis model.loadedseries )
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
            [ HA.class "series-table"
            ]
            ( List.map
                ( rowSeries model )
                ( Dict.toList model.loadedseries )
            )

rowSeries: Model -> ( String, SeriesAndInfos ) -> H.Html Msg
rowSeries model ( name, info ) =
    H.tr
        [ HA.id ( "remove-" ++ name )
        , HE.onMouseOver (Highlight name)
        , HE.onMouseLeave (Highlight "no-highlight")
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
                , HE.onClick ( ToggleAxis name )
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
                                Set.member name model.fixHighlighted
                                then "active"
                                else ""
                            )
                , HE.onClick ( FixedHighlight name )
                ]
                [ H.text "Highlight" ]
            ]
        , H.td
            []
            [ H.button
                [ HA.class "button-table-series"
                , HA.class "btn btn-warning"
                , HE.onClick ( RemoveSeries name )
                ]
                [ H.text "Remove" ]
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
                                  ++ if hasSecondAxis model.loadedseries
                                    then " True"
                                    else " False "
                                  )
                        ]
        baskets = H.div
                    []
                    <| List.map
                        (\ b -> H.text b)
                        model.searchBasket.items
    in
        List.concat [ [ H.br [] []]
                    , legendStuff
                    , [ secondAxis ]
                    , [ baskets ]
                    ]


permalink: Model -> H.Html Msg
permalink model =
    let
        names = List.map
                    (\name -> UB.string "series" name)
                    ( Dict.keys model.loadedseries )
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
                ( model.loadedseries )


emptyLoaded: List String -> Dict String SeriesAndInfos
emptyLoaded onSecondAxis =
    Dict.fromList
        <| List.map
            (\ name -> ( name, { emptySeriesInfo | secondAxis = True }))
            onSecondAxis


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
                    , searchSeries = initSearch selected
                    , searchBasket = initSearch []
                    , selecting = if (List.isEmpty selected )
                                    then ModeSeries
                                    else NoMode
                    , loadedseries = emptyLoaded axis2
                    , highlighted = Nothing
                    , fixHighlighted = Set.empty
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
                            ])
               )

    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
