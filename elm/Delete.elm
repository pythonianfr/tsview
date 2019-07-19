{- Compilation :

    $ sed -e "s/%APP%/Delete/" \
    -e "s/%URLPREFIX%/http:\/\/tshistory.test.pythonian.fr/" \
    elm/index.html.in > tsdelete/index.html
    $ elm make --output tsdelete/elm.js elm/Delete.elm

   Running :

    $ cd elm
    $ elm reactor&

-}


module Delete exposing (main)

import Browser
import Common exposing (classes)
import Dict
import Html.Styled exposing (..)
import Http
import Json.Decode as Decode
import KeywordMultiSelector
import KeywordSelector
import Tachyons.Classes as T
import Time
import Url.Builder as UB


type alias Model =
    { urlPrefix : String
    , series : List String
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
                    UB.crossOrigin model.urlPrefix
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
        ctx =
            KeywordMultiSelector.Context
                model.searchString
                model.searchedSeries
                model.selectedSeries
    in
    article [ classes [ T.center, T.pt4, T.w_90 ] ]
        [ KeywordMultiSelector.view selectorConfig ctx ]


main : Program String Model Msg
main =
    let
        initialGet urlPrefix =
            Http.get
                { expect = Http.expectJson CatalogReceived (Decode.dict Decode.string)
                , url =
                    UB.crossOrigin urlPrefix
                        [ "series", "catalog" ]
                        []
                }

        init urlPrefix =
            ( Model urlPrefix [] "" [] [] Nothing, initialGet urlPrefix )

        sub model =
            Time.every 1000 (always MakeSearch)
    in
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = sub
        }
