module Delete exposing (main)

import Browser
import Common exposing (classes)
import Dict exposing(Dict, fromList, keys, values)
import Html.Styled exposing (..)
import Http
import Json.Decode as Decode
import Catalog
import KeywordSelector
import SeriesSelector
import Tachyons.Classes as T
import Time
import Url.Builder as UB


type alias Model =
    { urlPrefix : String
    , catalog : Catalog.Model
    , search : SeriesSelector.Model
    , errors : Maybe (List String)
    }


type Msg
    = GotCatalog Catalog.Msg
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | OnDelete
    | DeleteDone (Result String String)
    | ToggleMenu
    | KindChange String Bool
    | SourceChange String Bool


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
            GotCatalog catmsg ->
                let
                    newcat = Catalog.update catmsg model.catalog
                    newsearch = SeriesSelector.fromcatalog model.search newcat
                in
                    newModel { model
                                 | catalog = newcat
                                 , search = newsearch
                             }

            ToggleMenu ->
                newModel { model | search = SeriesSelector.togglemenu model.search }

            KindChange kind checked ->
                let
                    newsearch = SeriesSelector.updatekinds
                                model.search
                                model.catalog
                                kind
                                checked
                in
                    newModel { model | search = newsearch }

            SourceChange source checked ->
                let
                    newsearch = SeriesSelector.updatesources
                                model.search
                                model.catalog
                                source
                                checked
                in
                    newModel { model | search = newsearch }

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
                                     model.search.filteredseries)
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
                    newsearch = SeriesSelector.new -- there must be a more elegant way
                                (removeItem x model.search.filteredseries)
                                model.search.search
                                (removeItem x model.search.found)
                                (removeItem x model.search.selected)
                                model.search.menu
                                model.search.kinds
                                model.search.sources
                in
                    newModel
                    { model
                        | catalog = Catalog.removeSeries x model.catalog
                        , search = newsearch
                    }

            DeleteDone (Err x) ->
                newModel -- why not trigger GotCatalog again ?
                { model
                    | errors = Just <| Common.maybe [ x ] ((::) x) model.errors
                }


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
    , onMenuToggle = ToggleMenu
    , onKindChange = KindChange
    , onSourceChange = SourceChange
    , divAttrs = [ classes [ T.aspect_ratio, T.aspect_ratio__1x1, T.mb4 ] ]
    }


view : Model -> Html Msg
view model =
    let
        viewError xs =
            ul [] (List.map (\x -> li [] [ text x ]) xs)
    in
        case model.errors of
            Nothing ->
                article [ classes [ T.center, T.pt4, T.w_90 ] ]
                    [ SeriesSelector.view model.search model.catalog selectorConfig ]

            Just error ->
                viewError error


main : Program String Model Msg
main =
    let
        init urlprefix =
            let
                prefix = Common.checkUrlPrefix urlprefix
            in
                ( Model
                      prefix
                      (Catalog.new Dict.empty)
                      SeriesSelector.null
                      Nothing
                ,
                      Cmd.map GotCatalog (Catalog.get prefix)
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
