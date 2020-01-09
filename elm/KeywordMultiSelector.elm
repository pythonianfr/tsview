module KeywordMultiSelector exposing (Config, Context, view)

import Common exposing (classes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (autofocus, value, placeholder)
import Html.Styled.Events exposing (onInput)
import ItemSelector
import Tachyons.Classes as T


type alias Config msg =
    { searchSelector : ItemSelector.Config msg
    , actionSelector : ItemSelector.Config msg
    , onInputMsg : String -> msg
    , divAttrs : List (Attribute msg)
    }


type alias Context msg =
    { searchString : String
    , searchedItems : List String
    , selectedItems : List String
    , errorMessage : Maybe (Html msg)
    }


view : Config msg -> Context msg -> Html msg
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
            , placeholder "start typing here ..."
            ] []

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

        addErr mess =
            let
                cls =
                    classes
                    [ T.flex
                    , T.items_center
                    , T.pa4
                    , T.bg_washed_red
                    , T.navy
                    ]
            in
                [ div [ cls ] [ mess ] ]

        checkErr xs =
            Common.maybe xs (addErr >> List.append xs) ctx.errorMessage
    in
        div cfg.divAttrs
            [ searchInput
            -- XXX w_90 should not be there but how to fix it ?
            , div [ classes [ T.w_90, T.absolute, T.z_2, T.bg_white_80 ] ] <| checkErr [ cols ]
            ]
