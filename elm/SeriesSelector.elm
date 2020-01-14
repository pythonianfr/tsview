module SeriesSelector exposing
    (Model
    , new
    , null
    , fromcatalog
    , updatesearch
    , updatefound
    , updateselected
    , updatekinds
    , Config
    , View
    , view
    )

import Catalog
import Common exposing (classes)
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing
    (autofocus
    , checked
    , for
    , id
    , name
    , type_
    , value
    , placeholder
    )
import Html.Styled.Events exposing
    (onCheck
    , onInput
    )
import ItemSelector
import Set exposing (Set)
import Tachyons.Classes as T


-- model

type alias Model =
    { filteredseries : List String -- list of series to select from
    , search : String  -- the search input pattern
    , found : List String  -- series matching the search
    , selected : List String  --  selected series
    , kinds : List String -- list of series kinds
    }


new search searched selected kinds =
    Model search searched selected kinds


null =
    new [] "" [] [] []


updatesearch : Model -> String -> Model
updatesearch model newsearch =
    { model | search = newsearch }


updatefound : Model -> List String -> Model
updatefound model newfound =
    { model | found = newfound }


updateselected : Model -> List String -> Model
updateselected model newselection =
    { model | selected = newselection }


fromcatalog : Model -> Catalog.Model -> Model
fromcatalog model catalog =
    let
        newkinds = List.sort (Dict.keys catalog.seriesByKind)
        seriesbykind kind =
            Set.toList (Maybe.withDefault Set.empty (Dict.get kind catalog.seriesByKind))
        allserieslist =
            List.map seriesbykind newkinds
    in
        { model
            | kinds = newkinds
            , filteredseries = List.sort (List.concat allserieslist)
        }


filterseries : Model -> Catalog.Model -> List String
filterseries model catalog =
    let
        seriesbykind kind =
            Set.toList (Maybe.withDefault Set.empty (Dict.get kind catalog.seriesByKind))
        allserieslist =
            List.map seriesbykind model.kinds
    in
        List.sort (List.concat allserieslist)


updatekinds : Model -> Catalog.Model -> String -> Bool -> Model
updatekinds model catalog kind checked =
    let
        newkinds =
            if
                checked
            then
                List.sort (kind :: model.kinds)
            else
                List.filter (\x -> x /= kind) model.kinds
        newmodel = { model | kinds = newkinds }
    in
        { newmodel | filteredseries = filterseries newmodel catalog }


-- view

type alias Config msg =
    { searchSelector : ItemSelector.Config msg
    , actionSelector : ItemSelector.Config msg
    , onInputMsg : String -> msg
    , onKindChange : String -> Bool -> msg
    , divAttrs : List (Attribute msg)
    }


type alias View msg =
    { search : Model
    , errorMessage : Maybe (Html msg)
    }


filterdiv : String -> List String -> (String -> Bool -> msg) -> Html msg
filterdiv filtername activenames event =
    div [] [ input [ type_ "checkbox"
                   , id filtername
                   , name filtername
                   , checked (List.member filtername activenames)
                   , onCheck (event filtername)
                   ] []
           , label [ for filtername ] [ text filtername ]
           ]


makefilter : String -> List String -> List String -> (String -> Bool -> msg) -> Html msg
makefilter section sectionitems activeitems event =
    div []
        [ p []
          (
           [ text ("series " ++ section) ] ++
              List.map
              (\x -> filterdiv x activeitems event)
              sectionitems
          ) ]


view : Model -> Catalog.Model -> Config msg -> View msg -> Html msg
view model catalog cfg viewctx =
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
            , makefilter "kinds" (Dict.keys catalog.seriesByKind) model.kinds cfg.onKindChange
            -- XXX w_90 should not be there but how to fix it ?
            , div [ classes [ T.w_90, T.absolute, T.z_2, T.bg_white_80 ] ] <| checkErr [ cols ]
            ]
