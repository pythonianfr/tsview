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

        new x = ( x, Cmd.none )

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
                    new { model
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
                    new { model | search = newsearch }

            SourceChange source checked ->
                let
                    newsearch = SeriesSelector.updatesources
                                model.search
                                model.catalog
                                source
                                checked
                in
                    new { model | search = newsearch }

            ToggleItem x ->
                new { model | search = SeriesSelector.updateselected
                                            model.search
                                            (toggleitem x model.search.selected)
                         }

            SearchSeries x ->
                new { model | search = SeriesSelector.updatesearch
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
                    new { model | search = newsearch }

            EditMode ->
                new
                { model
                    | state = Edit
                    , renamed = Maybe.withDefault "" (List.head model.search.selected)
                }

            SelectMode ->
                new { model | state = Select, error = Nothing }

            NewSerieName x ->
                new { model | renamed = x, error = Nothing }

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
                , Cmd.map GotCatalog (Catalog.get model.urlprefix 0)
                )

            RenameDone (Err x) ->
                new { model | error = Just "something wrong happened" }


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
                { attrs = [ ]
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
            H.div [] [ H.a [ HE.onMouseDown OnRename ] [ H.text "Rename" ]
                     , H.a [ HE.onMouseDown SelectMode ] [ H.text "Cancel" ]
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
        H.article [ ] [ content ]


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
                Cmd.map GotCatalog (Catalog.get prefix 0)
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
