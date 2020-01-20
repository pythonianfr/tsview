module SeriesSelector exposing
    (Model
    , new
    , null
    , fromcatalog
    , updatesearch
    , updatefound
    , updateselected
    , updatekinds
    , updatesources
    , SelectorConfig
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
    , classList
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
    , onMouseDown
    )
import Set exposing (Set)
import Tachyons.Classes as T


-- model

type alias Model =
    { filteredseries : List String -- list of series to select from
    , search : String  -- the search input pattern
    , found : List String  -- series matching the search
    , selected : List String  --  selected series
    , kinds : List String -- list of series kinds
    , sources : List String -- list of series sources
    }


new search searched selected kinds sources =
    Model search searched selected kinds sources


null =
    new [] "" [] [] [] []


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
        newsources = List.sort (Dict.keys catalog.seriesBySource)
    in
        { model
            | kinds = newkinds
            , sources = newsources
            , filteredseries = List.sort catalog.series
        }


filterseries : Model -> Catalog.Model -> List String
filterseries model catalog =
    let
        seriesbykind kind =
            Set.toList (Maybe.withDefault Set.empty (Dict.get kind catalog.seriesByKind))
        filteredseries =
            List.concat (List.map seriesbykind model.kinds)
        filterbysource source =
            let
                series = Maybe.withDefault Set.empty (Dict.get source catalog.seriesBySource)
            in
                List.filter (\x -> Set.member x series) filteredseries
    in
        List.sort (List.concat (List.map filterbysource model.sources))


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


updatesources : Model -> Catalog.Model -> String -> Bool -> Model
updatesources model catalog source checked =
    let
        newsources =
            if
                checked
            then
                List.sort (source :: model.sources)
            else
                List.filter (\x -> x /= source) model.sources
        newmodel = { model | sources = newsources }
    in
        { newmodel | filteredseries = filterseries newmodel catalog }


-- view

-- item selector


type alias ItemConfig msg =
    { action :
          Maybe
          { attrs : List (Attribute msg)
          , html : Html msg
          , clickMsg : msg
          }
    , defaultText : Html msg
    , toggleMsg : String -> msg
    }


type alias ItemContext =
    { items : List String
    , selectedItems : List String
    }


viewitemselector : ItemConfig msg -> ItemContext -> Html msg
viewitemselector cfg ctx =
    let
        enabled = List.length ctx.items > 0

        ulClass =
            classes [ T.list, T.pl0, T.ml0, T.w_100, T.ba, T.b__light_silver, T.br3 ]

        liAttrs item =
            let
                liClass =
                    classes [ T.ph3, T.pv2, T.bb, T.b__light_silver, T.dim ]

                liSelected =
                    let
                        isSelected = List.member item ctx.selectedItems
                    in
                        classList <|
                            List.map
                                (\x -> ( x, isSelected ))
                                [ T.white, T.bg_blue ]
            in
                [ liClass, liSelected, onMouseDown <| cfg.toggleMsg item ]

        lst =
            ul [ ulClass ] <|
                List.map
                    (\x -> li (liAttrs x) [ text x ])
                    ctx.items

        lstWithAction act =
            let
                aClass =
                    classes
                    [ T.w_100, T.link, T.dim, T.ph3, T.pv2, T.ma2, T.dib, T.tc ]

                attrs =
                    [ aClass, onMouseDown act.clickMsg ] ++ act.attrs
            in
                div [ classes [ T.dt, T.dt__fixed ] ] [ a attrs [ act.html ], lst ]
    in
        if
            enabled
        then
            Maybe.map lstWithAction cfg.action |> Maybe.withDefault lst
        else
            div [ classes [ T.pl0, T.ml0 ] ] [ cfg.defaultText ]


-- series selector

type alias SelectorConfig msg =
    { searchSelector : ItemConfig msg
    , actionSelector : ItemConfig msg
    , onInputMsg : String -> msg
    , onKindChange : String -> Bool -> msg
    , onSourceChange : String -> Bool -> msg
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


view : Model -> Catalog.Model -> SelectorConfig msg -> View msg -> Html msg
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
                    viewitemselector
                        selectorCfg
                        (ItemContext items viewctx.search.selected)
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
            , makefilter "sources" (Dict.keys catalog.seriesBySource) model.sources cfg.onSourceChange
            -- XXX w_90 should not be there but how to fix it ?
            , div [ classes [ T.w_90, T.absolute, T.z_2, T.bg_white_80 ] ] <| checkErr [ cols ]
            ]
