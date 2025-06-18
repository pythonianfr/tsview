module Homepage exposing (main)

import Browser
import Dict exposing ( Dict )
import Html as H
import Html.Attributes as HA
import Http
import Icons exposing
    ( Icon
    , buildSvg
    , getIcons
    )
import Json.Decode as JD
import Metadata as M
import Set exposing ( Set )
import Url.Builder as UB
import Util as U


type Status
    = Processing


type alias MultiStatus =
    { catalog : Status }


type alias Model =
    { baseurl : String
    , instance : String
    , version : String
    , status : MultiStatus
    , stats : Dict String M.Metadata
    , icons : Dict String Icon
    }


sourcessum: Dict String M.Metadata -> String -> Int
sourcessum stats key =
    -- sum things using the key, from all sources
    let
        sumbysource source =
            case Maybe.withDefault (M.MInt 0) <| Dict.get key source of
                M.MInt val -> val
                _ -> 0

    in
    List.foldl (+) 0 <| List.map sumbysource <| Dict.values stats


type Msg
    = GotInfo (Result Http.Error String)
    | GotIcons (Result Http.Error (Dict String Icon))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInfo (Ok rawinfo) ->
            case JD.decodeString (JD.dict M.decodemeta) rawinfo of
                Ok info ->
                    U.nocmd { model | stats = info }
                Err _ ->
                    U.nocmd model

        GotInfo (Err _) ->
            U.nocmd model

        GotIcons (Ok content) ->
            U.nocmd { model | icons = content }

        GotIcons (Err error) ->
            U.nocmd model


buildDetailsDiv : Model -> H.Html Msg
buildDetailsDiv model =
    let
        x=Debug.log "S" model.stats
        primaries =
            sourcessum model.stats "primary_series"

        formulas =
            sourcessum model.stats "formula_series"

        allseries =
            primaries + formulas

        sources =
            List.filter (\item -> item /= "local") <| Dict.keys model.stats
    in
    H.div
        [ HA.class "homepage-details" ]
        [ H.h1
            []
            [ H.u
                []
                [ H.text model.instance ]
            , H.text " refinery"
            ]
        , H.img
            [ HA.src ("https://img.shields.io/badge/refinery-" ++ model.version ++ "-35845F") ]
            []
        , H.br [] []
        , H.p [] [ H.text "From here, you can access :" ]
        , H.ul []
            [ H.p
                []
                [ H.a
                    [ HA.href "/tssearch" ]
                    [ H.text <| (String.fromInt allseries) ++ " Series" ]
                ]
            , H.p
                []
                [ H.text (" ↳ " ++ (String.fromInt primaries) ++ " Primaries") ]
            , H.p
                []
                [ H.text (" ↳ " ++ (String.fromInt formulas) ++ " Formulas") ]
            ]
        , H.p
            [ HA.hidden <| List.length (Dict.keys model.stats) <= 1 ]
            [ H.text "Sources :" ]
        , H.p []
            [ H.text (String.join " / " sources) ]
        ]


buildLinksDiv : Model -> H.Html Msg
buildLinksDiv model =
    H.div
        [ HA.id "links" ]
        [ H.a
            [ HA.class "homepage-button"
            , HA.class "apibutton"
            , HA.target "_blank"
            , HA.href (model.baseurl ++ "/api")
            ]
            [ H.img [ HA.src "./tsview_static/arrow_outward.png" ] []
            , H.text "API"
            ]
        , H.br [] []
        , H.a
            [ HA.class "homepage-button"
            , HA.class "greenbutton"
            , HA.target "_blank"
            , HA.href "https://refinery.docs.pythonian.fr"
            ]
            [ H.text "documentation" ]
        , H.br [] []
        , H.a
            [ HA.class "homepage-button"
            , HA.class "redbutton"
            , HA.target "_blank"
            , HA.href "mailto: contact@pythonian.fr"
            ]
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
            [ HA.class "cards-grid-container" ]
            [ H.div [ HA.class "homepage-card card1" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg model.icons "bi bi-download" ]
                , H.br [] []
                , H.div
                    [ HA.class "homepage-card-body" ]
                    [ H.h4 [ HA.class "homepage-card-title" ] [ H.text "Import data" ]
                    , H.br [] []
                    , H.p
                        [ HA.class "homepage-card-text" ]
                        [ H.text
                            "Data can be imported through the "
                        , H.a [ HA.href (model.baseurl ++ "/api") ] [ H.text "API." ]
                        ]
                    , H.p
                        [ HA.class "homepage-card-text" ]
                        [ H.text
                            "To facilitate imports, a python client is available as well as an excel client plugin. "
                        , H.a
                            [ HA.href "mailto: contact@pythonian.fr" ]
                            [ H.text "Contact Pythonian" ]
                        , H.text
                            " to have access to tutorials."
                        ]
                    , H.p
                        [ HA.class "homepage-card-text" ]
                        [ H.text
                            "If tasks have been constructed to import data (scraping open data on the internet, csv files to be ingested…), go to the "
                        , H.a
                            [ HA.href (model.baseurl ++ "/tasks") ]
                            [ H.text "task launcher." ]
                        ]
                    ]
                ]
            , H.div [ HA.class "homepage-card card2" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg model.icons "bi bi-speedometer" ]
                , H.br [] []
                , H.div
                    [ HA.class "homepage-card-body" ]
                    [ H.h4 [ HA.class "homepage-card-title" ] [ H.text "Get to know your data" ]
                    , H.br [] []
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "To have the complete list of the data available in this refinery, go to the "
                        , H.a
                            [ HA.href (model.baseurl ++ "/tssearch") ]
                            [ H.text "catalog." ]
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "From here, there is a redirection for each series to its own information page. There, every interesting information will be found (tzawareness, metadata, last updates, version history… and obviously… a plot!)."
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.a [ HA.href (model.baseurl ++ "/tsview") ] [ H.text "Quick view " ]
                        , H.text
                            " will be useful to compare several series on a plot and share the permalinks with colleagues."
                        ]
                    ]
                ]
            , H.div [ HA.class "homepage-card card3" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg model.icons "bi bi-tools" ]
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
                        , H.a [ HA.href (model.baseurl ++ "/tsformula/operators") ] [ H.text " here. " ]
                        , H.text
                            "The "
                        , H.a [ HA.href (model.baseurl ++ "/tsformula") ] [ H.text "formula editor" ]
                        , H.text
                            " is useful to construct one formula. If a batch of formulas has to be pushed, prefer the "
                        , H.a [ HA.href (model.baseurl ++ "/addformulas") ] [ H.text "csv solution." ]
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.text
                            "If a monster formulas has been constructed (with hundreds of dependencies…), don’t hesitate to setup a "
                        , H.a
                            [ HA.href (model.baseurl ++ "/formulacache") ]
                            [ H.text "cache policy!" ]
                        ]
                    ]
                ]
            , H.div [ HA.class "homepage-card card4" ]
                [ H.div
                    [ HA.class "homepage-card-img-top" ]
                    [ buildSvg model.icons "bi bi-heart-pulse" ]
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
                        , H.a [ HA.href (model.baseurl ++ "/tswatch") ] [ H.text "tswatch " ]
                        , H.text
                                " to check if the data is correctly updated."
                        ]
                    , H.p [ HA.class "homepage-card-text" ]
                        [ H.a [ HA.href (model.baseurl ++ "/tasks") ] [ H.text "Task " ]
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
        [ ]
        [ H.div
            [ HA.class "main-content" ]
            [ H.div [ HA.class "homepage-content" ]
                [ H.div
                    [ HA.id "homepage-home" ]
                    [ H.div
                        [ HA.class "flex-container timeseriesrefinery home-container" ]
                        [ buildDetailsDiv model
                        , H.br [] []
                        ]
                    , H.img [ HA.class "home-image", HA.src "./tsview_static/grid.png" ] []
                    , buildLinksDiv model
                    ]
                , buildGetStartedDiv model
                , H.div
                    [ HA.id "homepage-footer" ]
                    [ H.img [ HA.class "pythonian-logo"
                            , HA.src "./tsview_static/logo-pythonian.png"
                            ] []
                    ]
                ]
            ]
        ]



init : ( String, String, String ) -> ( Model, Cmd Msg )
init ( baseurl, instance, version ) =
    let
        model =
            { baseurl = baseurl
            , instance = instance
            , version = version
            , status = { catalog = Processing }
            , stats = Dict.empty
            , icons = Dict.empty
            }
    in
    ( model
    , Cmd.batch
        [ U.getinfo model GotInfo
        , getIcons baseurl <| \returnHttp ->  GotIcons returnHttp
        ]
    )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
