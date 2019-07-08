{- Compilation :

    $ elm make --output elm/delete_elm.js elm/Delete.elm

   Running :

    $ cd elm
    $ elm reactor&

-}


module Delete exposing (main)

import Browser
import Common exposing (classes)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, classList)
import Html.Styled.Events exposing (onInput, onMouseDown)
import Http
import ItemSelector
import Json.Decode as Decode
import KeywordSelector
import Tachyons.Classes as T
import Time
import Url.Builder as UB


type alias Model =
    { series : List String
    , searchString : String
    , searchedSeries : List String
    , selectedSeries : List String
    , status : Maybe String
    }


type alias SeriesCatalog =
    Dict.Dict String String


type Msg
    = CatalogReceived (Result Http.Error SeriesCatalog)
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | OnDelete
    | DeleteDone (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        removeItem x xs =
            List.filter ((/=) x) xs

        toggleItem x xs =
            if List.member x xs then
                removeItem x xs

            else
                x :: xs

        newModel x =
            ( x, Cmd.none )

        keywordMatch xm xs =
            if String.length xm < 2 then
                []

            else
                KeywordSelector.select xm xs |> List.take 20

        delete expect url =
            Http.request
                { method = "DELETE"
                , headers = []
                , url = url
                , body = Http.emptyBody
                , expect = expect
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    case msg of
        CatalogReceived (Ok x) ->
            let
                series =
                    Dict.keys x
            in
            newModel { model | series = series }

        CatalogReceived (Err _) ->
            newModel { model | status = Just "Error on CatalogReceived" }

        ToggleItem x ->
            newModel { model | selectedSeries = toggleItem x model.selectedSeries }

        SearchSeries x ->
            newModel { model | searchString = x }

        MakeSearch ->
            newModel { model | searchedSeries = keywordMatch model.searchString model.series }

        OnDelete ->
            let
                expect =
                    Http.expectJson DeleteDone Decode.string

                mkUrl serieName =
                    UB.crossOrigin "http://tshistory.test.pythonian.fr"
                        [ "series", "state" ]
                        [ UB.string "name" serieName ]
            in
            ( model, Cmd.batch <| List.map (mkUrl >> delete expect) model.selectedSeries )

        DeleteDone (Ok x) ->
            let
                _ =
                    Debug.log "Result on DeleteDone" x
            in
            newModel
                { model
                    | series = removeItem x model.series
                    , searchedSeries = removeItem x model.searchedSeries
                    , selectedSeries = removeItem x model.selectedSeries
                }

        DeleteDone (Err x) ->
            let
                _ =
                    Debug.log "Error on DeleteDone" x
            in
            newModel model


searchSelectorConfig : ItemSelector.Config Msg
searchSelectorConfig =
    { action = Nothing
    , defaultText = text "Type some keywords in input bar for selecting time series"
    , toggleMsg = ToggleItem
    }


actionSelectorConfig : ItemSelector.Config Msg
actionSelectorConfig =
    { action =
        Just
            { attrs = [ classes [ T.white, T.bg_light_red ] ]
            , html = text "Delete"
            , clickMsg = OnDelete
            }
    , defaultText = text ""
    , toggleMsg = ToggleItem
    }


view : Model -> Html Msg
view model =
    let
        articleClass =
            classes [ T.center, T.pt4, T.w_90 ]

        divClass =
            classes [ T.aspect_ratio, T.aspect_ratio__1x1, T.mb4 ]

        fuzzySelector =
            let
                searchInput =
                    let
                        inputClass =
                            classes
                                [ T.input_reset
                                , T.dtc
                                , T.ba
                                , T.b__black_20
                                , T.pa2
                                , T.db
                                , T.w_100
                                ]
                    in
                    [ input [ inputClass, onInput SearchSeries ] [] ]

                cols =
                    let
                        attrs =
                            [ classes [ T.dtc, T.pa1 ] ]

                        render ( cfg, series ) =
                            ItemSelector.view
                                cfg
                                (ItemSelector.Context series model.selectedSeries)
                    in
                    List.map
                        (\x -> div attrs [ render x ])
                        [ ( searchSelectorConfig, model.searchedSeries )
                        , ( actionSelectorConfig, model.selectedSeries )
                        ]
            in
            List.map
                (div [ classes [ T.dt, T.dt__fixed ] ])
                [ searchInput, cols ]
    in
    article [ articleClass ] [ div [ divClass ] fuzzySelector ]


main : Program () Model Msg
main =
    let
        initialGet =
            Http.get
                { expect = Http.expectJson CatalogReceived (Decode.dict Decode.string)
                , url = "http://tshistory.test.pythonian.fr/series/catalog"
                }

        init _ =
            ( Model [] "" [] [] Nothing, initialGet )

        sub model =
            Time.every 1000 (always MakeSearch)
    in
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = sub
        }
