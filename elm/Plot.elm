module Plot exposing (main)

import Browser
import Common exposing (classes)
import Dict exposing (Dict)
import Html
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Catalog
import Json.Decode as Decode
import KeywordSelector
import Plotter exposing (getplotdata, scatterplot, seriesdecoder, plotargs, Series)
import SeriesSelector
import Time
import Url.Builder as UB
import Util as U


type alias Model =
    { prefix : String
    , catalog: Catalog.Model
    , haseditor : Bool
    , search : SeriesSelector.Model
    , selecting : Bool
    , loadedseries : Dict String Series
    , mindate : String
    , maxdate : String
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
    | FvdatePickerChanged String
    | TvdatePickerChanged String


dateof strdate =
    case String.split "T" strdate of
        head::_ ->
            head
        [] -> ""


fetchseries model =
    let
        selected = model.search.selected
        ismissing series =
            not <| Dict.member series model.loadedseries
        missing = List.filter ismissing selected
    in List.map
        (\name -> getplotdata
             model.prefix name Nothing (GotPlotData name) 0 model.mindate model.maxdate
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
        removeItem x xs = List.filter ((/=) x) xs
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
                    newcat = Catalog.update catmsg model.catalog
                    newsearch = SeriesSelector.fromcatalog model.search newcat
                in
                    U.nocmd { model
                                | catalog = newcat
                                , search = newsearch
                            }

            KindChange kind checked ->
                let
                    newsearch = SeriesSelector.updatekinds
                                model.search
                                model.catalog
                                kind
                                checked
                in
                    U.nocmd { model | search = newsearch }

            SourceChange source checked ->
                let
                    newsearch = SeriesSelector.updatesources
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
                    newmodel = { model
                                   | search = SeriesSelector.updateselected
                                     model.search
                                     (toggleItem x model.search.selected)
                               }
                in
                    ( newmodel
                    , Cmd.batch <| fetchseries newmodel
                    )

            SearchSeries x ->
                let
                    search = SeriesSelector.updatesearch model.search x
                in
                    U.nocmd { model | search = search }

            MakeSearch ->
                let
                    search = SeriesSelector.updatefound model.search
                             (keywordMatch
                                  model.search.search
                                  model.search.filteredseries)
                in
                    U.nocmd { model | search = search }

            -- plot

            GotPlotData name (Ok rawdata) ->
                case Decode.decodeString seriesdecoder rawdata of
                    Ok val ->
                        let loaded = Dict.insert name val model.loadedseries
                        in U.nocmd { model | loadedseries = loaded }

                    Err err ->
                        doerr "gotplotdata decode" <| Decode.errorToString err

            GotPlotData name (Err err) ->
                doerr "gotplotdata error" <| U.unwraperror err

            -- dates

            FvdatePickerChanged value ->
                let
                    newmodel = { model
                                   | mindate = value
                                   , loadedseries = Dict.empty
                               }
                in
                    ( newmodel
                    , Cmd.batch <| fetchseries newmodel
                    )

            TvdatePickerChanged value ->
                let
                    newmodel = { model
                                   | maxdate = value
                                   , loadedseries = Dict.empty
                               }
                in
                    ( newmodel
                    , Cmd.batch <| fetchseries newmodel
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
        , H.a [HA.href <| UB.relative [ "tsinfo" ] [ UB.string "name" seriesName]
              , HA.target "_blank"
              ]
              [ H.text <| "info" ]
        , H.text " "
        , H.a [ HA.href <| UB.relative [ "tshistory", seriesName ] []
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
    [ H.span [ ] [ H.text " " ]
    , H.label [ HA.for "fvd-picker" ] [ H.text "from value date" ]
    , H.span [ ] [ H.text " " ]
    , H.input [ HA.type_ "date"
              , HA.id "fvd-picker"
              , HA.name "fvd-picker"
              , HA.value model.mindate
              , HE.onInput FvdatePickerChanged
              ] [ ]
    , H.span [ ] [ H.text " " ]
    , H.label [ HA.for "tvd-picker" ] [ H.text "to value date" ]
    , H.span [ ] [ H.text " " ]
    , H.input [ HA.type_ "date"
            , HA.id "tvd-picker"
            , HA.name "tvd-picker"
            , HA.value model.maxdate
            , HE.onInput TvdatePickerChanged
            ] [ ]
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
                        )
                        model.search.selected
            in
                plotargs plotDiv data

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
                                (List.map
                                     (\x -> UB.string "series" x)
                                     model.search.selected
                                )
                    in
                        H.a [ HA.href url ] [ H.text "Permalink" ]

                links =
                    List.map
                        (viewlinks model.haseditor)
                        model.search.selected

            in
                H.ul [ ]
                    (List.map
                         (\x -> H.li [ ] [ x ])
                         (permalink :: links)
                    )
    in
        H.div []
            [ H.header [ ] [ selector ]
            , H.div [ HA.id plotDiv ] []
            , viewdatepicker model
            , plotFigure [ HA.attribute "args" args ] []
            , H.footer [] [ urls ]
            ]


main : Program
       { prefix : String
       , selected : List String
       , haseditor : Bool
       } Model Msg
main =
    let
        init flags =
            let
                selected = flags.selected
                model = Model
                        flags.prefix
                        (Catalog.new Dict.empty)
                        flags.haseditor
                        (SeriesSelector.new [] "" [] selected [] [])
                        (List.isEmpty selected)
                        Dict.empty
                        ""
                        ""
                        []
            in ( model
               , Cmd.batch <| [
                      Cmd.map GotCatalog (Catalog.get model.prefix 1)
                     ] ++ fetchseries model
               )

        sub model =
            -- this is a cheap (cadenced) debouncer for the search ui
            if model.selecting then
                Time.every 1000 (always MakeSearch)

            else
                Sub.none
    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
