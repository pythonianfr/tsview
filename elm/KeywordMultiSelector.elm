module KeywordMultiSelector exposing (Config, Context, view)

import Common exposing (classes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (autofocus)
import Html.Styled.Events exposing (onInput)
import ItemSelector
import Tachyons.Classes as T


type alias Config msg =
    { searchSelector : ItemSelector.Config msg
    , actionSelector : ItemSelector.Config msg
    , onInputMsg : String -> msg
    , divAttrs : List (Attribute msg)
    }


type alias Context =
    { searchedItems : List String
    , selectedItems : List String
    }


view : Config msg -> Context -> Html msg
view cfg ctx =
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
            [ input [ inputClass, onInput cfg.onInputMsg, autofocus True ] [] ]

        cols =
            let
                attrs =
                    [ classes [ T.dtc, T.pa1 ] ]

                render ( selectorCfg, items ) =
                    ItemSelector.view
                        selectorCfg
                        (ItemSelector.Context items ctx.selectedItems)
            in
            List.map
                (\x -> div attrs [ render x ])
                [ ( cfg.searchSelector, ctx.searchedItems )
                , ( cfg.actionSelector, ctx.selectedItems )
                ]
    in
    div cfg.divAttrs
        (List.map
            (div [ classes [ T.dt, T.dt__fixed ] ])
            [ searchInput, cols ]
        )
