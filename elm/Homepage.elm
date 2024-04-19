module Homepage exposing (main)

import Browser
import Catalog as Cat exposing (Msg(..))
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Menu as Men exposing (iconesDefinition)
import Set exposing (Set)
import Svg exposing (path, svg)
import Svg.Attributes
    exposing
        ( d
        , fill
        , fillRule
        , viewBox
        )


type Status
    = Processing


type alias MultiStatus =
    { catalog : Status }


type alias Model =
    { baseUrl : String
    , instance : String
    , version : String
    , status : MultiStatus
    , catalog : Cat.Model
    , menu : Men.Model
    }


type Msg
    = GotCatalog Cat.Msg
    | Menu Men.Msg


nocmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCatalog catmsg ->
            nocmd { model | catalog = Cat.update catmsg model.catalog }

        Menu menumsg ->
            nocmd { model | menu = Men.updateModel menumsg model.menu }


buildSvg icones iconeName =
    let
        icone =
            Maybe.withDefault
                []
                (Dict.get iconeName icones)
    in
    svg
        [ viewBox "0 0 16 16"
        , fill "currentColor"
        ]
        (List.map
            (\ipath -> buildSvgPath ipath)
            icone
        )


buildSvgPath ipath =
    case ipath.fillRule of
        Nothing ->
            path [ d ipath.d ] []

        Just rule ->
            path [ fillRule rule, d ipath.d ] []

getSeriesNumberOf model seriestype =
    String.fromInt
        (List.length
            (Set.toList
                (Maybe.withDefault
                    Set.empty
                    (Dict.get seriestype model.catalog.seriesbykind))))

buildDetailsDiv : Model -> H.Html Msg
buildDetailsDiv model =
    H.div
        [ HA.class "homepage-details" ]
        [ H.h1
            []
            [ H.u
                []
                [ H.text model.instance ]
            , H.text " refinery"
            ]
        , H.img [ HA.src ("https://img.shields.io/badge/refinery-" ++ model.version ++ "-35845F") ] []
        , H.br [] []
        , H.p [] [ H.text "From here, you can access :" ]
        , H.ul []
            [ H.p
                []
                [ H.text (String.fromInt (List.length model.catalog.series) ++ " Series") ]
            , H.p
                []
                [ H.text (" ↳ " ++ (getSeriesNumberOf model "primary") ++ " Primaries") ]
            , H.p
                []
                [ H.text (" ↳ " ++ (getSeriesNumberOf model "formula") ++ " Formulas") ]
            ]
        , H.p [ HA.hidden (Dict.keys model.catalog.seriesbysource == []) ] [ H.text "Sources :" ]
        , H.p []
            [ H.text (String.join " / " (Dict.keys model.catalog.seriesbysource)) ]
        ]


buildLinksDiv : Model -> H.Html Msg
buildLinksDiv model =
    H.div
        [ HA.id "links" ]
        [ H.a
            [ HA.class "homepage-button"
            , HA.class "apibutton"
            , HA.target "_blank"
            , HA.href (model.baseUrl ++ "/api") ]
            [ H.img [ HA.src "./tsview_static/arrow_outward.png" ] []
            , H.text "API" ]
        , H.br [] []
        , H.a
            [ HA.class "homepage-button"
            , HA.class "greenbutton"
            , HA.target "_blank"
            , HA.href "https://tshistory-refinery.readthedocs.io/en/latest/" ]
            [ H.text "documentation" ]
        , H.br [] []
        , H.a
            [ HA.class "homepage-button"
            , HA.class "redbutton"
            , HA.target "_blank"
            , HA.href "mailto: contact@pythonian.fr" ]
            [ H.text "Contact Pythonian" ]
        ]


buildGetStartedDiv : Model -> H.Html Msg
buildGetStartedDiv model =
    H.div
        [ HA.id "homepage-getstarted" ]
        [ H.p
            [ HA.class "greentext" ]
            [ H.text "The power of the Timeseries Refinery" ]
        , H.h2 [] [ H.text "Get Started" ]
        , H.div
            [ HA.class "flex-container-center" ]
            [ H.div [ HA.class "homepage-card" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg iconesDefinition "bi bi-download" ]
                , H.br [] []
                , H.div
                    [ HA.class "homepage-card-body" ]
                    [ H.h4 [ HA.class "homepage-card-title" ] [ H.text "Import data" ]
                    , H.br [] []
                    , H.p
                        [ HA.class "homepage-card-text" ]
                        [ H.text
                            "Data can be imported through the "
                        , H.a [ HA.href (model.baseUrl ++ "/api") ] [ H.text "API." ]
                        ]
                    , H.p
                        [ HA.class "homepage-card-text" ]
                        [ H.text
                            "To facilitate imports, a python client is available as well as an excel client plugin. "
                        , H.a [ HA.href "mailto: contact@pythonian.fr" ] [ H.text "Contact Pythonian" ]
                        , H.text
                            " to have access to tutorials."
                        ]
                    , H.p
                        [ HA.class "homepage-card-text" ]
                        [ H.text
                            "If tasks have been constructed to import data (scraping open data on the internet, csv files to be ingested…), go to the "
                        , H.a [ HA.href (model.baseUrl ++ "/tasks") ] [ H.text "task launcher." ]
                        ]
                    ]
                ]
            , H.div [ HA.class "homepage-card" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg iconesDefinition "bi bi-speedometer" ]
                , H.br [] []
                , H.div
                    [ HA.class "homepage-card-body" ]
                    [ H.h4 [ HA.class "homepage-card-title" ] [ H.text "Get to know your data" ]
                    , H.br [] []
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "To have the complete list of the data available in this refinery, go to the "
                        , H.a [ HA.href (model.baseUrl ++ "/tssearch") ] [ H.text "catalog." ]
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "From here, there is a redirection for each series to its own information page. There, every interesting information will be found (tzawareness, metadata, last updates, version history… and obviously… a plot!)."
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.a [ HA.href (model.baseUrl ++ "/tsview") ] [ H.text "Quick view " ]
                        , H.text
                            " will be useful to compare several series on a plot and share the permalinks with colleagues."
                        ]
                    ]
                ]
            , H.div [ HA.class "homepage-card" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg iconesDefinition "bi bi-tools" ]
                , H.br [] []
                , H.div
                    [ HA.class "homepage-card-body" ]
                    [ H.h4 [ HA.class "homepage-card-title" ] [ H.text "Correct and transform" ]
                    , H.br [] []
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "An outlier has been spotted ? A manual (and versioned!) correction is possible from the info page of the series (go to « edit values »)."
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "For series transformation, welcome to the world of formulas ! Operator documentation is available "
                        , H.a [ HA.href (model.baseUrl ++ "/tsformula/operators") ] [ H.text " here. " ]
                        , H.text
                            "The "
                        , H.a [ HA.href (model.baseUrl ++ "/tsformula") ] [ H.text "formula editor" ]
                        , H.text
                            " is useful to construct one formula. If a batch of formulas has to be pushed, prefer the "
                        , H.a [ HA.href (model.baseUrl ++ "/addformulas") ] [ H.text "csv solution." ]
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "If a monster formulas has been constructed (with hundreds of dependencies…), don’t hesitate to setup a "
                        , H.a [ HA.href (model.baseUrl ++ "/formulacache") ] [ H.text "cache policy!" ]
                        ]
                    ]
                ]
            , H.div [ HA.class "homepage-card" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg iconesDefinition "bi bi-heart-pulse" ]
                , H.br [] []
                , H.div
                    [ HA.class "homepage-card-body" ]
                    [ H.h4 [ HA.class "homepage-card-title" ] [ H.text "Monitor refinery health" ]
                    , H.br [] []
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "Is everything ok with this refinery ?"
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                                "Go to "
                        , H.a [ HA.href (model.baseUrl ++ "/tswatch") ] [ H.text "tswatch " ]
                        , H.text
                                " to check if the data is correctly updated."
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.a [ HA.href (model.baseUrl ++ "/tasks") ] [ H.text "Task " ]
                        , H.text
                                "page will be also useful to check if all the recent tasks are «done» (tips: it is possible to filter the «status» column)."
                        ]
                    ]
                ]
            ]
        ]


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.class
            (if model.menu.menuModeText then
                "grid-container-text"

             else
                "grid-container-icon"
            )
        ]
        [ Men.viewMenu model.menu Menu
        , H.div
            [ HA.class "main-content"
            , HA.style "margin" ".5em"
            ]
            [ H.div [ HA.class "homepage-content" ]
                [ H.div
                    [ HA.id "homepage-home" ]
                    [ H.div
                        [ HA.class "flex-container timeseriesrefinery home-container" ]
                        [ buildDetailsDiv model
                        , H.br [] []
                        , H.img [ HA.class "home-plot", HA.src "./tsview_static/plot.png" ] [] ]
                    , H.img [ HA.class "home-image", HA.src "./tsview_static/grid.png" ] []
                    , buildLinksDiv model
                    ]
                , buildGetStartedDiv model
                , H.div
                    [ HA.id "homepage-footer" ]
                    [ H.img [ HA.class "pythonian-logo", HA.src "./tsview_static/logo-pythonian.png" ] [] ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


initModel baseurl instance version =
    { baseUrl = baseurl
    , instance = instance
    , version = version
    , status = { catalog = Processing }
    , catalog = Cat.empty
    , menu =
        { menuContent = Men.contentMenu
        , menuModeText = False
        , icones = Men.iconesDefinition
        }
    }


init : ( String, String, String ) -> ( Model, Cmd Msg )
init ( baseurl, instance, version ) =
    ( initModel baseurl instance version
    , Cmd.map GotCatalog <| Cat.get baseurl "series" 1 Cat.ReceivedSeries
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
