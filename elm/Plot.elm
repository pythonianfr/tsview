module Plot exposing (main)

import Browser
import Common exposing (classes)
import Dict exposing (Dict)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as A
import Html.Styled.Events exposing (onClick)
import Http
import Catalog
import Json.Decode as Decode
import KeywordSelector
import Plotter exposing (getplotdata, scatterplot, seriesdecoder, plotargs, Series)
import SeriesSelector
import Tachyons.Classes as T
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
    | ToggleMenu


fetchseries model =
    let
        selected = model.search.selected
        ismissing series =
            not <| Dict.member series model.loadedseries
        missing = List.filter ismissing selected
    in List.map
        (\name -> getplotdata model.prefix name Nothing (GotPlotData name) 0 "" "")
        missing


plotFigure : List (Attribute msg) -> List (Html msg) -> Html msg
plotFigure =
    node "plot-figure"


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

            ToggleMenu ->
                U.nocmd { model | search = SeriesSelector.togglemenu model.search }

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


selectorConfig : SeriesSelector.SelectorConfig Msg
selectorConfig =
    { searchSelector =
        { action = Nothing
        , defaultText =
            text
                "Type some keywords in input bar for selecting time series"
        , toggleMsg = ToggleItem
        }
    , actionSelector =
        { action = Nothing
        , defaultText = text ""
        , toggleMsg = ToggleItem
        }
    , onInputMsg = SearchSeries
    , onMenuToggle = ToggleMenu
    , onKindChange = KindChange
    , onSourceChange = SourceChange
    , divAttrs = [ classes [ T.mb4 ] ]
    }


viewlinks cls haseditor seriesName =
    div [ ]
        [ text (seriesName ++ " ")
        , a [A.href <| UB.relative [ "tsinfo" ] [ UB.string "name" seriesName]
            , A.target "_blank"
            , cls
            ]
              [ text <| "info" ]
        , text " "
        , a [ A.href <| UB.relative [ "tshistory", seriesName ] []
            , A.target "_blank"
            , cls
            ]
              [ text <| "history" ]
        , text " "
        , if haseditor then
              a [ A.href <| UB.relative [ "tseditor/?name=" ++ seriesName ] []
                , A.target "_blank"
                , cls
                ]
              [ text <| "editor" ]
          else
              text ""
        ]


view : Model -> Html Msg
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
                cls = classes [ T.pb2, T.f4, T.fw6, T.db, T.navy, T.link, T.dim ]
                children =
                    [ a
                      [ cls, onClick ToggleSelection, A.title "click to toggle selector" ]
                      [ text "Series selection" ]
                    ]
            in
                form [ classes [ T.center, T.pt4, T.w_90 ] ]
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
                cls = classes [ T.link, T.blue, T.lh_title ]
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
                        a [ A.href url, cls ] [ text "Permalink" ]

                links =
                    List.map
                        (viewlinks cls model.haseditor)
                        model.search.selected

            in
                ul [ classes [ T.list, T.mt3, T.mb0 ] ]
                    (List.map
                         (\x -> li [ classes [ T.pv2 ] ] [ x ])
                         (permalink :: links)
                    )
    in
        div []
            [ header [ classes [ T.bg_light_blue ] ] [ selector ]
            , div [ A.id plotDiv ] []
            , plotFigure [ A.attribute "args" args ] []
            , footer [] [ urls ]
            ]


main : Program
       { prefix : String
       , selectedSeries : List String
       , haseditor : Bool
       } Model Msg
main =
    let
        init flags =
            let
                selected = flags.selectedSeries
                model = Model
                        flags.prefix
                        (Catalog.new Dict.empty)
                        flags.haseditor
                        (SeriesSelector.new [] "" [] selected False [] [])
                        (List.isEmpty selected)
                        Dict.empty
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
            , view = view >> toUnstyled
            , update = update
            , subscriptions = sub
            }
