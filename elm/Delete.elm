module Delete exposing (main)

import Browser
import Common exposing (classes)
import Dict exposing(Dict, fromList, keys, values)
import Html.Styled exposing (..)
import Http
import Json.Decode as Decode
import Catalog exposing (RawSeriesCatalog, seriesFromCatalog, kindsFromCatalog)
import KeywordMultiSelector
import KeywordSelector
import Tachyons.Classes as T
import Time
import Url.Builder as UB


type alias Model =
    { urlPrefix : String
    , series : List String
    , sources : List String
    , seriesKind : Dict String String
    , searchString : String
    , searchedSeries : List String
    , selectedSeries : List String
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
                newModel { model
                             | series = seriesFromCatalog x
                             , sources = keys x
                             , seriesKind = kindsFromCatalog x
                         }

            CatalogReceived (Err x) ->
                newModel { model | errors = Just [ x ] }

            ToggleItem x ->
                newModel { model | selectedSeries = toggleItem x model.selectedSeries }

            SearchSeries x ->
                newModel { model | searchString = x }

            MakeSearch ->
                newModel { model
                             | searchedSeries = keywordMatch model.searchString model.series
                         }

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
                    , Cmd.batch <| List.map (mkUrl >> delete expect) model.selectedSeries
                    )

            DeleteDone (Ok x) ->
                newModel
                { model
                    | series = removeItem x model.series
                    , searchedSeries = removeItem x model.searchedSeries
                    , selectedSeries = removeItem x model.selectedSeries
                }

            DeleteDone (Err x) ->
                newModel
                { model
                    | errors = Just <| Common.maybe [ x ] ((::) x) model.errors
                }


selectorConfig : KeywordMultiSelector.Config Msg
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
            KeywordMultiSelector.Context
                model.searchString
                model.searchedSeries
                model.selectedSeries
                (Maybe.map viewError model.errors)
    in
        article [ classes [ T.center, T.pt4, T.w_90 ] ]
            [ KeywordMultiSelector.view selectorConfig ctx ]


main : Program String Model Msg
main =
    let
        initialGet urlPrefix =
            Http.get
                { expect =
                      Common.expectJsonMessage
                      CatalogReceived
                      (Decode.dict (Decode.list (Decode.list Decode.string)))
                , url =
                    UB.crossOrigin urlPrefix
                        [ "api", "series", "catalog" ]
                        []
                }

        init urlPrefix =
            let
                p = Common.checkUrlPrefix urlPrefix
            in
                ( Model p [] [] (fromList []) "" [] [] Nothing, initialGet p )

        sub model =
            Time.every 1000 (always MakeSearch)
    in
        Browser.element
            { init = init
            , view = view >> toUnstyled
            , update = update
            , subscriptions = sub
            }
