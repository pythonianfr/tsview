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
    , view
    )

import Catalog
import Common exposing (classes)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Set exposing (Set)


-- model

type alias Model =
    { filteredseries : List String -- list of series to select from
    , search : String  -- the search input pattern
    , found : List String  -- series matching the search
    , selected : List String  --  selected series
    , kinds : List String -- list of series kinds
    , sources : List String -- list of series sources
    }


new series search found selected kinds sources =
    Model series search found selected kinds sources


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
          { attrs : List (H.Attribute msg)
          , html : H.Html msg
          , clickMsg : msg
          }
    , defaultText : H.Html msg
    , toggleMsg : String -> msg
    }



viewitemselector : ItemConfig msg -> List String -> List String -> H.Html msg
viewitemselector cfg items selected =
    let
        liAttrs item =
            let
                liSelected =
                    let
                        isSelected = List.member item selected
                    in
                        HA.classList <|
                            List.map
                                (\x -> ( x, isSelected ))
                                [ "white", "blue" ]
            in
                [ liSelected, HE.onMouseDown <| cfg.toggleMsg item ]

        lst =
            H.ul [ HA.class "list-group" ] <|
                List.map
                    (\x -> H.li ([ HA.class "list-group-item" ] ++ (liAttrs x)) [ H.text x ])
                    items

        lstWithAction act =
            let
                attrs =
                    [ HE.onMouseDown act.clickMsg ] ++ act.attrs
            in
                H.div [ ] [ H.a attrs [ act.html ], lst ]
    in
        if
            List.length items > 0
        then
            Maybe.map lstWithAction cfg.action |> Maybe.withDefault lst
        else
            H.div [ ] [ cfg.defaultText ]


-- series selector

type alias SelectorConfig msg =
    { searchSelector : ItemConfig msg
    , actionSelector : ItemConfig msg
    , onInputMsg : String -> msg
    , onKindChange : String -> Bool -> msg
    , onSourceChange : String -> Bool -> msg
    , divAttrs : List (H.Attribute msg)
    }


filterdiv : String -> List String -> (String -> Bool -> msg) -> H.Html msg
filterdiv filtername activenames event =
    H.div [ ]
        [ H.input [ HA.type_ "checkbox"
                  , HA.id filtername
                  , HA.name filtername
                  , HA.checked (List.member filtername activenames)
                  , HE.onCheck (event filtername)
                  ] []
        , H.label [ HA.for filtername ] [ H.text filtername ]
        ]


makefilter : String -> List String -> List String -> (String -> Bool -> msg) -> H.Html msg
makefilter section sectionitems activeitems event =
    H.div [ ]
        [ H.p [ ]
          (
           [ H.text ("Series " ++ section) ] ++
              List.map
              (\x -> filterdiv x activeitems event)
              sectionitems
          ) ]


view : Model -> Catalog.Model -> SelectorConfig msg -> H.Html msg
view model catalog cfg =
    let
        searchInput =
            H.div [ ] [
                 H.input
                     [ HA.value model.search
                     , HE.onInput cfg.onInputMsg
                     , HA.autofocus True
                     , HA.placeholder
                         ("start typing here to pick from " ++
                              (String.fromInt (List.length model.filteredseries)) ++
                              " items"
                           )
                     ] []
                ]

        selectorwidget =
            let
                renderselector (selectorCfg, items) =
                    viewitemselector selectorCfg items model.selected
            in
                H.div [ ]
                    (List.map
                         (\x -> H.div [ ] [ renderselector x ])
                         [ ( cfg.searchSelector, model.found )
                         , ( cfg.actionSelector, model.selected )
                         ]
                    )

    in
        H.div cfg.divAttrs
            [ searchInput
            , selectorwidget
            ]
