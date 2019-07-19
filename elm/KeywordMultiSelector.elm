module KeywordMultiSelector exposing (Config, Context, view)

import Common exposing (classes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (autofocus, value)
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
    { searchString : String
    , searchedItems : List String
    , selectedItems : List String
    }


view : Config msg -> Context -> Html msg
view cfg ctx =
    let
        searchInput =
            input
                [ classes
                    [ T.input_reset
                    , T.ba
                    , T.b__black_20
                    , T.pa2
                    , T.db
                    , T.w_100
                    ]
                , value ctx.searchString
                , onInput cfg.onInputMsg
                , autofocus True
                ]
                []

        cols =
            let
                render ( selectorCfg, items ) =
                    ItemSelector.view
                        selectorCfg
                        (ItemSelector.Context items ctx.selectedItems)
            in
            div [ classes [ T.dt, T.dt__fixed ] ]
                (List.map
                    (\x -> div [ classes [ T.dtc, T.pa1 ] ] [ render x ])
                    [ ( cfg.searchSelector, ctx.searchedItems )
                    , ( cfg.actionSelector, ctx.selectedItems )
                    ]
                )
    in
    div cfg.divAttrs
        [ searchInput

        -- XXX w_90 should not be there but how to fix it ?
        , div [ classes [ T.w_90, T.absolute, T.z_2, T.bg_white_80 ] ] [ cols ]
        ]
