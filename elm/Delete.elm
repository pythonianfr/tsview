module Delete exposing (main)

import Browser
import Common exposing (classes)
import Dict exposing(Dict, fromList, keys, values)
import Html as H
import Html.Attributes as HA
import Http
import Json.Decode as Decode
import Catalog
import KeywordSelector
import SeriesSelector
import Time
import Url.Builder as UB


type alias Model =
    { urlPrefix : String
    , catalog : Catalog.Model
    , search : SeriesSelector.Model
    , errors : Maybe (List String) -- why many ?
    }


type Msg
    = GotCatalog Catalog.Msg
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | OnDelete
    | DeleteDone String (Result Http.Error String)
    | KindChange String Bool
    | SourceChange String Bool


-- deletion update helpers
stopupdating model =
    ( model, Cmd.none )


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


deleteseries model =
    case model.search.selected of
        [] -> stopupdating ( model )-- we're done

        head :: tail ->
            let
                mkurl name = UB.crossOrigin model.urlPrefix
                             [ "api", "series", "state" ]
                             [ UB.string "name" name ]
                expect = Http.expectString (DeleteDone head)
            in
                ( model
                , delete expect (mkurl head)
                )


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

        keywordMatch xm xs =
            if String.length xm < 2 then
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
                    stopupdating { model
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
                    stopupdating { model | search = newsearch }

            SourceChange source checked ->
                let
                    newsearch = SeriesSelector.updatesources
                                model.search
                                model.catalog
                                source
                                checked
                in
                    stopupdating { model | search = newsearch }

            ToggleItem x ->
                let
                    newsearch = SeriesSelector.updateselected
                                model.search
                                (toggleItem x model.search.selected)
                in
                    stopupdating { model | search = newsearch }

            SearchSeries x ->
                let
                    newsearch = SeriesSelector.updatesearch model.search x
                in
                    stopupdating { model | search = newsearch }

            MakeSearch ->
                let
                    newsearch = SeriesSelector.updatefound
                                model.search
                                (keywordMatch
                                     model.search.search
                                     model.search.filteredseries)
                in
                    stopupdating { model | search = newsearch }

            OnDelete ->
                deleteseries model

            DeleteDone name (Ok _) ->
                let
                    newsearch = SeriesSelector.new -- there must be a more elegant way
                                (removeItem name model.search.filteredseries)
                                model.search.search
                                (removeItem name model.search.found)
                                (removeItem name model.search.selected)
                                model.search.kinds
                                model.search.sources
                    newmodel = { model
                                   | catalog = Catalog.removeSeries name model.catalog
                                   , search = newsearch
                               }
                in
                    -- this will void model.search.selected one name at a time
                    deleteseries newmodel

            DeleteDone series (Err _) ->
                let
                    err = "stopping, wrong signal on delete for " ++ series
                in
                    stopupdating
                    { model
                        | errors = Just <| Common.maybe [ err ] ((::) err) model.errors
                    }


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
          { action =
                Just
                { attrs = [ HA.class "btn btn-danger" ]
                , html = H.text "Delete"
                , clickMsg = OnDelete
                }
          , defaultText = H.text ""
          , toggleMsg = ToggleItem
          }
    , onInputMsg = SearchSeries
    , onKindChange = KindChange
    , onSourceChange = SourceChange
    , divAttrs = [ ]
    }


view : Model -> H.Html Msg
view model =
    let
        viewError xs =
            H.ul [] (List.map (\x -> H.li [] [ H.text x ]) xs)
    in
        case model.errors of
            Nothing ->
                H.article [ ]
                    [ SeriesSelector.view model.search model.catalog selectorConfig ]

            Just error ->
                viewError error


main : Program String Model Msg
main =
    let
        init prefix =
            ( Model
                  prefix
                  Catalog.empty
                  SeriesSelector.null
                  Nothing
            ,
                Cmd.map GotCatalog (Catalog.get prefix 0)
            )

        sub model =
            Time.every 1000 (always MakeSearch)
    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
