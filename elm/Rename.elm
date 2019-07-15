{- Compilation :

    $ elm make --output elm/rename_elm.js elm/Rename.elm

   Running :

    $ cd elm
    $ elm reactor&

-}


module Rename exposing (main)

import Browser
import Common exposing (classes)
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onInput, onMouseDown)
import Http
import Json.Decode as Decode
import KeywordSelector
import KeywordSingleSelector
import Tachyons.Classes as T
import Time
import Url.Builder as UB


type State
    = Select
    | Edit


type alias Model =
    { state : State
    , series : List String
    , searchString : String
    , searchedSeries : List String
    , selectedSerie : Maybe String
    , renamedSerie : String
    , status : Maybe String
    }


type alias SeriesCatalog =
    Dict.Dict String String


type Msg
    = CatalogReceived (Result Http.Error SeriesCatalog)
    | ToggleItem String
    | SearchSeries String
    | MakeSearch
    | SelectMode
    | EditMode
    | NewSerieName String
    | OnRename
    | RenameDone (Result String String)


getCatalog : Cmd Msg
getCatalog =
    Http.get
        { expect = Http.expectJson CatalogReceived (Decode.dict Decode.string)
        , url =
            UB.crossOrigin "http://tshistory.test.pythonian.fr"
                [ "series", "catalog" ]
                []
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        removeItem x xs =
            List.filter ((/=) x) xs

        toggleItem new maybeCurrent =
            case maybeCurrent of
                Just current ->
                    if current == new then
                        Nothing

                    else
                        Just new

                Nothing ->
                    Just new

        newModel x =
            ( x, Cmd.none )

        keywordMatch xm xs =
            if String.length xm < 2 then
                []

            else
                KeywordSelector.select xm xs |> List.take 20
    in
    case msg of
        CatalogReceived (Ok x) ->
            let
                series =
                    Dict.keys x
            in
            ( { model
                | series = series
                , searchedSeries = keywordMatch model.searchString model.series
              }
            , Cmd.none
            )

        CatalogReceived (Err x) ->
            let
                _ =
                    Debug.log "Error on CatalogReceived" x
            in
            newModel model

        ToggleItem x ->
            newModel { model | selectedSerie = toggleItem x model.selectedSerie }

        SearchSeries x ->
            newModel { model | searchString = x }

        MakeSearch ->
            newModel
                { model
                    | searchedSeries = keywordMatch model.searchString model.series
                }

        EditMode ->
            newModel
                { model
                    | state = Edit
                    , renamedSerie = Maybe.withDefault "" model.selectedSerie
                }

        SelectMode ->
            newModel { model | state = Select, status = Nothing }

        NewSerieName x ->
            newModel { model | renamedSerie = x, status = Nothing }

        OnRename ->
            let
                expect =
                    Common.expectJsonMessage RenameDone Decode.string

                url =
                    UB.crossOrigin "http://tshistory.test.pythonian.fr"
                        [ "series", "state" ]
                        [ UB.string "name" <|
                            Maybe.withDefault "" model.selectedSerie
                        , UB.string "newname" model.renamedSerie
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
                , selectedSerie = Nothing
                , renamedSerie = ""
                , status = Nothing
              }
            , getCatalog
            )

        RenameDone (Err x) ->
            newModel { model | status = Just x }


selectorConfig : KeywordSingleSelector.Config Msg
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
                { attrs = [ classes [ T.white, T.bg_blue ] ]
                , html = text "Rename"
                , clickMsg = EditMode
                }
        , defaultText = text ""
        , toggleMsg = ToggleItem
        }
    , onInputMsg = SearchSeries
    , divAttrs = [ classes [ T.aspect_ratio, T.aspect_ratio__1x1, T.mb4 ] ]
    }


editor : Model -> Html Msg
editor model =
    let
        edit =
            let
                txt =
                    "New name for : "
                        ++ Maybe.withDefault "" model.selectedSerie

                lab =
                    label
                        [ classes [ T.db, T.fw1, T.lh_copy ] ]
                        [ text txt ]

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

                inpt =
                    input
                        [ inputClass
                        , value model.renamedSerie
                        , onInput NewSerieName
                        ]
                        []
            in
            div [ classes [ T.mb3 ] ] [ lab, inpt ]

        button ( txt, bg_color, toMsg ) =
            let
                aClass =
                    classes
                        [ T.link
                        , T.dim
                        , T.ph3
                        , T.pv2
                        , T.ma2
                        , T.dib
                        , T.tc
                        , T.white
                        , bg_color
                        ]
            in
            a [ aClass, onMouseDown toMsg ] [ text txt ]

        buttonAttrs =
            [ ( "Rename", T.bg_blue, OnRename )
            , ( "Cancel", T.bg_light_red, SelectMode )
            ]

        buttons =
            div [] (List.map button buttonAttrs)

        addErr mess =
            let
                cls =
                    classes
                        [ T.flex
                        , T.items_center
                        , T.justify_center
                        , T.pa4
                        , T.bg_washed_red
                        , T.navy
                        ]
            in
            [ div [ cls ] [ text mess ] ]

        checkErr xs =
            Common.maybe xs (addErr >> List.append xs) model.status
    in
    div selectorConfig.divAttrs <| checkErr [ edit, buttons ]


view : Model -> Html Msg
view model =
    let
        ctx =
            KeywordSingleSelector.Context model.searchedSeries model.selectedSerie

        content =
            case model.state of
                Select ->
                    KeywordSingleSelector.view selectorConfig ctx

                Edit ->
                    editor model
    in
    article [ classes [ T.center, T.pt4, T.w_90 ] ] [ content ]


main : Program () Model Msg
main =
    let
        init _ =
            ( Model Select [] "" [] Nothing "" Nothing, getCatalog )

        sub model =
            if model.state == Select then
                Time.every 1000 (always MakeSearch)

            else
                Sub.none
    in
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = sub
        }
