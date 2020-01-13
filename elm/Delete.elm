module Delete exposing (main)

import Browser
import Common exposing (classes)
import Dict exposing(Dict, fromList, keys, values)
import Html.Styled exposing (..)
import Http
import Json.Decode as Decode
import Catalog exposing (RawSeriesCatalog, SeriesCatalog, buildCatalog, getCatalog, removeSeries)
import KeywordSelector
import SeriesSelector
import Tachyons.Classes as T
import Time
import Url.Builder as UB


type alias Model =
    { urlPrefix : String
    , catalog : SeriesCatalog
    , search : SeriesSelector.Model
    , errors : Maybe (List String)
    }


type Msg
    = CatalogReceived (Result String RawSeriesCatalog)
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | OnDelete
    | DeleteDone (Result String String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        removeItem x xs = List.filter ((/=) x) xs

        toggleItem x xs =
            if
                List.member x xs
            then
                removeItem x xs
            else
                x :: xs

        newModel x = ( x, Cmd.none )

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
                newModel { model | catalog = buildCatalog x }

            CatalogReceived (Err x) ->
                newModel { model | errors = Just [ x ] }

            ToggleItem x ->
                let
                    newsearch = SeriesSelector.updateselected
                                model.search
                                (toggleItem x model.search.selected)
                in
                    newModel { model | search = newsearch }

            SearchSeries x ->
                let
                    newsearch = SeriesSelector.updatesearch model.search x
                in
                    newModel { model | search = newsearch }

            MakeSearch ->
                let
                    newsearch = SeriesSelector.updatefound
                                model.search
                                (keywordMatch
                                     model.search.search
                                     model.catalog.series)
                in
                    newModel { model | search = newsearch }

            OnDelete ->
                let
                    expect =
                        Common.expectJsonMessage DeleteDone Decode.string

                    mkUrl serieName =
                        UB.crossOrigin model.urlPrefix
                            [ "api", "series", "state" ]
                            [ UB.string "name" serieName ]
                in
                    ( model
                    , Cmd.batch <| List.map (mkUrl >> delete expect) model.search.selected
                    )

            DeleteDone (Ok x) ->
                let
                    newsearch = SeriesSelector.new
                                model.search.search
                                (removeItem x model.search.found)
                                (removeItem x model.search.selected)
                in
                    newModel
                    { model
                        | catalog = removeSeries x model.catalog
                        , search = newsearch
                    }

            DeleteDone (Err x) ->
                newModel
                { model
                    | errors = Just <| Common.maybe [ x ] ((::) x) model.errors
                }


selectorConfig : SeriesSelector.Config Msg
selectorConfig =
    { searchSelector =
          { action = Nothing
          , defaultText =
              text
              "Type some keywords in input bar for selecting time series"
          , toggleMsg = ToggleItem
          }
    , actionSelector =
          { action =
                Just
                { attrs = [ classes [ T.white, T.bg_light_red ] ]
                , html = text "Delete"
                , clickMsg = OnDelete
                }
          , defaultText = text ""
          , toggleMsg = ToggleItem
          }
    , onInputMsg = SearchSeries
    , divAttrs = [ classes [ T.aspect_ratio, T.aspect_ratio__1x1, T.mb4 ] ]
    }


view : Model -> Html Msg
view model =
    let
        viewError xs =
            ul [] (List.map (\x -> li [] [ text x ]) xs)

        ctx =
            SeriesSelector.View
                model.search
                (Maybe.map viewError model.errors)
    in
        article [ classes [ T.center, T.pt4, T.w_90 ] ]
            [ SeriesSelector.view selectorConfig ctx ]


main : Program String Model Msg
main =
    let
        initialGet urlPrefix =
            getCatalog urlPrefix (Common.expectJsonMessage CatalogReceived)

        init urlPrefix =
            let
                prefix = Common.checkUrlPrefix urlPrefix
            in
                ( Model
                      prefix
                      (buildCatalog Dict.empty)
                      SeriesSelector.null
                      Nothing
                ,
                      initialGet prefix
                )

        sub model =
            Time.every 1000 (always MakeSearch)
    in
        Browser.element
            { init = init
            , view = view >> toUnstyled
            , update = update
            , subscriptions = sub
            }
