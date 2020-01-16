module SeriesSelector exposing
    (Model
    , new
    , null
    , updatesearch
    , updatefound
    , updateselected
    , Config
    , View
    , view
    )

import Common exposing (classes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (autofocus, value, placeholder)
import Html.Styled.Events exposing (onInput)
import ItemSelector
import Tachyons.Classes as T


-- model

type alias Model =
    { search : String  -- the search input pattern
    , found : List String  -- series matching the search
    , selected : List String  --  selected series
    }


new search searched selected =
    Model search searched selected


null =
    new "" [] []


updatesearch model newsearch =
    { model | search = newsearch }


updatefound model newfound =
    { model | found = newfound }


updateselected model newselection =
    { model | selected = newselection }


-- view

type alias Config msg =
    { searchSelector : ItemSelector.Config msg
    , actionSelector : ItemSelector.Config msg
    , onInputMsg : String -> msg
    , divAttrs : List (Attribute msg)
    }


type alias View msg =
    { search : Model
    , errorMessage : Maybe (Html msg)
    }


view : Config msg -> View msg -> Html msg
view cfg viewctx =
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
            , value viewctx.search.search
            , onInput cfg.onInputMsg
            , autofocus True
            , placeholder "start typing here ..."
            ] []

        cols =
            let
                render ( selectorCfg, items ) =
                    ItemSelector.view
                        selectorCfg
                        (ItemSelector.Context items viewctx.search.selected)
            in
                div [ classes [ T.dt, T.dt__fixed ] ]
                    (List.map
                         (\x -> div [ classes [ T.dtc, T.pa1 ] ] [ render x ])
                         [ ( cfg.searchSelector, viewctx.search.found )
                         , ( cfg.actionSelector, viewctx.search.selected )
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
            Common.maybe xs (addErr >> List.append xs) viewctx.errorMessage
    in
        div cfg.divAttrs
            [ searchInput
            -- XXX w_90 should not be there but how to fix it ?
            , div [ classes [ T.w_90, T.absolute, T.z_2, T.bg_white_80 ] ] <| checkErr [ cols ]
            ]
