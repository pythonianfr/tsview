module Rename exposing (main)

import Browser
import Common
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Catalog
import Json.Decode as Decode
import KeywordSelector
import SeriesSelector
import Time
import Url.Builder as UB
import Util as U


type State
    = Select
    | Edit


type alias Model =
    { urlprefix : String
    , state : State
    , catalog : Catalog.Model
    , search : SeriesSelector.Model
    , renamed : String
    , error : Maybe String
    }


type Msg
    = GotCatalog Catalog.Msg
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | SelectMode
    | EditMode
    | NewSerieName String
    | OnRename
    | RenameDone (Result Http.Error String)
    | KindChange String Bool
    | SourceChange String Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        toggleitem elt list =
            if
                List.member elt list
            then
                []
            else
                List.singleton elt

        keywordmatch xm xs =
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
                U.nocmd { model | search = SeriesSelector.updateselected
                                           model.search
                                           (toggleitem x model.search.selected)
                        }

            SearchSeries x ->
                U.nocmd { model | search = SeriesSelector.updatesearch
                                           model.search
                                           x
                        }

            MakeSearch ->
                let
                    newsearch = SeriesSelector.updatefound
                                model.search
                                (keywordmatch
                                     model.search.search
                                     model.search.filteredseries)
                in
                    U.nocmd { model | search = newsearch }

            EditMode ->
                U.nocmd
                { model
                    | state = Edit
                    , renamed = Maybe.withDefault "" (List.head model.search.selected)
                }

            SelectMode ->
                U.nocmd { model | state = Select, error = Nothing }

            NewSerieName x ->
                U.nocmd { model | renamed = x, error = Nothing }

            OnRename ->
                let
                    expect =
                        Http.expectString RenameDone

                    url =
                        UB.crossOrigin model.urlprefix
                            [ "api", "series", "state" ]
                            [ UB.string "name" <|
                                  Maybe.withDefault "" (List.head model.search.selected)
                            , UB.string "newname" model.renamed
                            ]

                    putRequest =
                        Http.request
                            { method = "PUT"
                            , headers = []
                            , url = url
                            , body = Http.emptyBody
                            , expect = expect
                            , timeout = Nothing
                            , tracker = Nothing
                            }
                in
                    ( model, putRequest )

            RenameDone (Ok _) ->
                ( { model
                      | state = Select
                      , search = SeriesSelector.null
                      , renamed = ""
                      , error = Nothing
                  }
                , Cmd.map GotCatalog (Catalog.get model.urlprefix "series" 0 Catalog.ReceivedSeries)
                )

            RenameDone (Err x) ->
                U.nocmd { model | error = Just "something wrong happened" }


selectorconfig : SeriesSelector.SelectorConfig Msg
selectorconfig =
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
                { attrs = [ HA.class "btn btn-primary" ]
                , html = H.text "Rename"
                , clickMsg = EditMode
                }
          , defaultText = H.text ""
          , toggleMsg = ToggleItem
          }
    , onInputMsg = SearchSeries
    , onKindChange = KindChange
    , onSourceChange = SourceChange
    , divAttrs = [ ]
    }


vieweditor : Model -> H.Html Msg
vieweditor model =
    let
        item = Maybe.withDefault "" (List.head model.search.selected)

        edit =
            H.div [] [ H.label [] [ H.text <| "New name for : " ++ item ]
                     , H.input [ HA.value model.renamed
                               , HE.onInput NewSerieName
                               ] []
                     ]

        buttons =
            H.div [] [ H.a [ HA.class "btn btn-warning"
                           , HE.onMouseDown OnRename ] [ H.text "Rename" ]
                     , H.a [ HA.class "btn btn-primary"
                           , HE.onMouseDown SelectMode ] [ H.text "Cancel" ]
                     ]

        adderr mess =
            [ H.div [ ] [ H.text mess ] ]

        checkerr xs =
            Common.maybe xs (adderr >> List.append xs) model.error
    in
        H.div selectorconfig.divAttrs <| checkerr [ edit, buttons ]


view : Model -> H.Html Msg
view model =
    let
        content =
            case model.state of
                Select ->
                    SeriesSelector.view model.search model.catalog selectorconfig

                Edit ->
                    vieweditor model
    in
        H.div [ HA.style "margin" "1em" ] [ content ]


main : Program String Model Msg
main =
    let
        init prefix =
            ( Model
                  prefix
                  Select
                  Catalog.empty
                  SeriesSelector.null
                  ""
                  Nothing
            ,
                Cmd.map GotCatalog (Catalog.get prefix "series" 0 Catalog.ReceivedSeries)
            )

        sub model =
            if
                model.state == Select
            then
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
