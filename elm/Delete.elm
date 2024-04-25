module Delete exposing (main)

import Browser
import Common
import Dict exposing(Dict, fromList, keys, values)
import Html as H
import Html.Attributes as HA
import Http
import Json.Decode as Decode
import Catalog
import KeywordSelector
import Menu as Men
import SeriesSelector
import Time
import Url.Builder as UB
import Util as U


type alias Model =
    { urlPrefix : String
    , menu : Men.Model
    , catalog : Catalog.Model
    , search : SeriesSelector.Model
    , errors : Maybe (List String) -- why many ?
    }


type Msg
    = Menu Men.Msg
    | GotCatalog Catalog.Msg
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | OnDelete
    | DeleteDone String (Result Http.Error String)
    | KindChange String Bool
    | SourceChange String Bool


-- deletion update helpers

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
        [] -> U.nocmd model -- we're done

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
            Menu menumsg ->
                ( { model | menu = Men.updateModel menumsg model.menu }
                , Men.buildCmd menumsg model.menu
                )

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

            ToggleItem x ->
                let
                    newsearch = SeriesSelector.updateselected
                                model.search
                                (toggleItem x model.search.selected)
                in
                    U.nocmd { model | search = newsearch }

            SearchSeries x ->
                let
                    newsearch = SeriesSelector.updatesearch model.search x
                in
                    U.nocmd { model | search = newsearch }

            MakeSearch ->
                let
                    newsearch = SeriesSelector.updatefound
                                model.search
                                (keywordMatch
                                     model.search.search
                                     model.search.filteredseries)
                in
                    U.nocmd { model | search = newsearch }

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
                    U.nocmd
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
                [
                    case model.errors of
                        Nothing ->
                            H.div
                                [ HA.style "margin" "1em" ]
                                [ SeriesSelector.view model.search model.catalog selectorConfig ]

                        Just error ->
                            viewError error
                ]
            ]


main : Program String Model Msg
main =
    let
        init prefix =
            ( Model
                  prefix
                  (Men.initmenu "timeseries-delete")
                  Catalog.empty
                  SeriesSelector.null
                  Nothing
            ,
               Cmd.batch
                    [ Men.getMenu prefix (\returnHttp -> Menu (Men.GotMenu returnHttp))
                    , Men.getIcons prefix (\returnHttp -> Menu (Men.GotIcons returnHttp))
                    , Cmd.map GotCatalog (Catalog.get prefix "series" 0 Catalog.ReceivedSeries)]
            )

        sub model =
             Sub.batch
                    [ Time.every 1000 (always MakeSearch)
                    , Men.loadMenuData (\str -> Menu (Men.LoadMenuData str))]
    in
        Browser.element
            { init = init
            , view = view
            , update = update
            , subscriptions = sub
            }
