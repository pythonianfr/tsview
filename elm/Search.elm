module Search exposing (main)

import Array
import AssocList as Assoc
import Browser
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , fromSeconds
    , provideInput
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
import Editor.Type as ET
import Editor.UI.Tree as UITree
import Editor.UI.Widget as Widget
import Filter exposing
    ( FilterNode(..)
    , Value(..)
    , fromlisp
    , parse
    , serialize
    )
import Finder as F
import Finder exposing
    ( Msg(..) )
import Html as H
import Html.Attributes as A
import Html.Events as HE
import Html.Keyed as K
import Html.Lazy as L
import Http
import Json.Decode as D
import Lisp
import List.Extra as LE
import Maybe.Extra as Maybe
import Metadata as M
import Set exposing (Set)
import Url.Builder as UB
import Util as U


type Mode =
    Series | Groups


type SearchMode =
    Basic | Expert


type alias BasketFormula =
    { code : String
    , node : FilterNode
    }


type alias Model =
    { baseurl : String
    , mode : Mode
    , searchmode : SearchMode
    , stats : Dict String M.Metadata
    -- base catalog elements
    , sources : List String
    , catalog : F.Model
    -- query
    , limit : Int
    , editlimit : Bool
    -- filter state (series)
    , selectedkinds : List String
    , selectedsources : List String
    -- filter state (all)
    , filterbyname : Maybe String
    , filterbyformula : Maybe String
    , tzaware : Maybe Bool
    -- filter state/metadata
    , filterbymeta : Dict Int (String, String)
    , errors : List String
    -- debouncing
    , namefilterdeb : Debouncer Msg
    , formulafilterdeb : Debouncer Msg
    -- expert
    , editorWidget : Widget.Model
    , basket: Maybe BasketFormula
    }


type Msg
    = GotSources (Result Http.Error String)
    | GotInfo (Result Http.Error String)
    | GotItemsDesc F.Msg
    | NameFilter String
    | FormulaFilter String
    | KindUpdated String
    | SourceUpdated String
    -- limit widget
    | EditLimit Bool
    | EditedLimit String
    -- metadata filter
    | NewValue Int String
    | NewKey Int String
    | AddMetaItem Int
    | MetaItemToDelete Int
    -- debouncer
    | DebounceNameFilter (Debouncer.Msg Msg)
    | DebounceFormulaFilter (Debouncer.Msg Msg)
    -- mode
    | ToggleMode
    | Tzaware String
    -- search mode
    | SetSearchMode SearchMode
    -- expert mode
    | WidgetMsg Widget.Msg


getsources baseurl =
    Http.get
        { expect =
              Http.expectString GotSources
        , url =
            UB.crossOrigin baseurl
                [ "api", "global", "properties" ]
                [ UB.string "property" "sources" ]
        }

sourcesdecoder =
    D.list D.string


insert list item =
    List.append list [item]


remove list item =
    List.filter ((/=) item) list


-- filters

query model =
    let
        quote = String.fromChar '"'

        bykinds =
            case model.selectedkinds of
                [ "primary" ] ->  [ "(by.not (by.formula))" ]
                [ "formula" ] -> [ "(by.formula)" ]
                [] -> [ "(by.formula)",  "(by.not (by.formula))" ] -- yeah :p
                _ -> []

        byname =
            case model.filterbyname of
                Nothing -> []
                Just fragment ->
                    let
                        frags =
                            String.split " " fragment
                        clause frag =
                            "(by.name " ++ quote ++ frag ++ quote ++ ")"
                    in
                    List.concat [ [ "(by.and " ]
                                , List.map clause frags
                                , [ ")" ]
                                ]

        byformula =
            case model.filterbyformula of
                Nothing -> []
                Just fragment ->
                    [ "(by.formulacontents " ++ quote ++ fragment ++ quote ++ ")" ]

        bytzaware =
            case model.tzaware of
                Nothing -> []
                Just aware -> case aware of
                    True -> [ "(by.tzaware)" ]
                    False -> [ "(by.not (by.tzaware))" ]

        filterfrommeta (key, value) =
            case (key, value) of
                ( "", "" ) ->
                    ""

                ( _, "" ) ->
                    "(by.metakey " ++ quote ++ key ++ quote ++ ")"

                _ ->
                    "(by.metaitem " ++
                        quote ++ key ++ quote ++ " " ++
                        quote ++ value ++ quote ++ ")"

        bymeta =
            List.filter (\item -> item /= "") <|
                List.map filterfrommeta <|
                    Dict.values model.filterbymeta

        together =
            List.concat [ bykinds, byname, byformula, bytzaware, bymeta ]

        expr =
            case List.length together of
                0 -> "(by.everything))"
                1 -> String.join " " together
                _ -> "(by.and " ++ (String.join " " together) ++ ")"

    in expr


doquery model =
    let
        cmd =
            case model.mode of
                Series ->
                    Cmd.map GotItemsDesc <|
                        F.find model.baseurl "series" F.ReceivedSeries
                            (query model) model.selectedsources model.limit
                Groups ->
                    Cmd.map GotItemsDesc <|
                        F.find model.baseurl "group" F.ReceivedGroups
                            (query model) model.selectedsources model.limit
    in
    ( model
    , cmd
    )


-- debouncing

updatednamefilterbouncer =
    { mapMsg = DebounceNameFilter
    , getDebouncer = .namefilterdeb
    , setDebouncer = \deb model -> { model | namefilterdeb = deb }
    }


updatedformulafilterbouncer =
    { mapMsg = DebounceFormulaFilter
    , getDebouncer = .formulafilterdeb
    , setDebouncer = \deb model -> { model | formulafilterdeb = deb }
    }

-- expert

tryfilter model { node } =
    Cmd.map GotItemsDesc <|
        F.find
            model.baseurl
            "series"
            F.ReceivedSeries
            ( Lisp.serialize <| serialize node )
            []
            1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSources (Ok rawsources) ->
            case D.decodeString sourcesdecoder rawsources of
                Ok sources ->
                    let
                        allsources =
                            List.concat [ [ "local" ], sources ]
                    in
                    U.nocmd { model
                                | sources = allsources
                                , selectedsources = allsources
                            }

                Err err ->
                    U.nocmd model

        GotSources (Err err) ->
            U.nocmd model

        GotInfo (Ok rawinfo) ->
            case D.decodeString (D.dict M.decodemeta) rawinfo of
                Ok info ->
                    U.nocmd { model | stats = info }
                Err _ ->
                    U.nocmd model

        GotInfo (Err _) ->
            U.nocmd model

        GotItemsDesc rawdesc ->
            let
                cat = F.update rawdesc model.catalog
                newmodel = { model | catalog = cat }
            in
            U.nocmd newmodel

        NameFilter value ->
            let
                filter = if value /= "" then Just value else Nothing
                newmodel = { model | filterbyname = filter }
            in
            doquery newmodel

        FormulaFilter value ->
            let
                filter = if value /= "" then Just value else Nothing
                newmodel = { model | filterbyformula = filter }
            in
            doquery newmodel

        KindUpdated kind ->
            let
                newkinds =
                    if List.member kind model.selectedkinds
                    then
                        if (List.length model.selectedkinds > 1)
                        then remove model.selectedkinds kind
                        -- let's do nothing: we need one kind
                        else model.selectedkinds
                    else insert model.selectedkinds kind

                newmodel =
                    { model | selectedkinds = List.sort newkinds }
            in
            doquery newmodel

        SourceUpdated source ->
            let
                newsources =
                    if List.member source model.selectedsources
                    then
                        (if List.length model.selectedsources > 1
                         then remove model.selectedsources source
                         -- let's do nothing: we want at least one source selected
                         else model.selectedsources
                        )
                    else insert model.selectedsources source

                newmodel =
                    { model | selectedsources = newsources }
            in
            doquery newmodel

        EditLimit editing ->
            let
                newmodel =
                    { model | editlimit = not model.editlimit }
            in
            case model.editlimit of
                False -> U.nocmd newmodel
                True -> doquery newmodel

        EditedLimit limit ->
            case limit of
                "" -> U.nocmd { model | limit = 0 }
                _ ->
                    case String.toInt limit of
                        Nothing ->
                            U.nocmd model
                        Just newlimit ->
                            U.nocmd { model | limit = newlimit }

        -- metadata filtering

        NewValue inputid value ->
            let
                currentkey =
                    U.first
                        (Maybe.withDefault
                             ( "", "" )
                             (Dict.get inputid model.filterbymeta)
                        )

                newdict =
                    Dict.insert
                        inputid
                        (currentkey, value)
                        model.filterbymeta

                newmodel =
                    { model | filterbymeta = newdict }
            in
            doquery newmodel

        NewKey inputid key ->
            let
                currentvalue =
                    U.snd
                        (Maybe.withDefault
                             ( "", "" )
                             (Dict.get inputid model.filterbymeta)
                        )

                newdict =
                    Dict.insert
                        inputid
                        (key, currentvalue)
                        model.filterbymeta

                newmodel =
                    { model | filterbymeta = newdict }
            in
            doquery newmodel

        AddMetaItem inputid ->
            let
                newdict =
                    Dict.insert
                        (inputid + 1)
                        ( "", "" )
                        model.filterbymeta

                newmodel =
                    { model | filterbymeta = newdict }
           in
           doquery newmodel

        MetaItemToDelete inputid ->
            let
                newdict =
                    Dict.remove inputid model.filterbymeta

                newmodel =  { model | filterbymeta = newdict }
            in
            doquery newmodel

        Tzaware value ->
            let
                aware =
                    case value of
                        "true" -> Just True
                        "false" -> Just False
                        _ -> Nothing

            in
            doquery { model | tzaware = aware }

        -- Debouncing

        DebounceNameFilter val ->
            Debouncer.update update updatednamefilterbouncer val model

        DebounceFormulaFilter val ->
            Debouncer.update update updatedformulafilterbouncer val model

        -- Mode

        ToggleMode ->
             doquery { model | mode =
                           case model.mode of
                               Series -> Groups
                               Groups -> Series
                     }

        -- search mode

        SetSearchMode newsearchmode ->
            let
                (wid, cmd) =
                    Widget.setFormula Nothing model.editorWidget

                editor =
                    model.editorWidget.savedModel.editionTree.editor

                byAndCmd =
                    Assoc.get
                        (ET.returnTypeFromString editor.returnTypeStr)
                        editor.spec
                    |> Maybe.andThen (Assoc.get "by.and")
                    |> Maybe.withDefault ET.voidOperator
                    |> UITree.SelectOperator
                    |> UITree.EditEntry
                    |> UITree.EditNode Array.empty
                    |> U.sendCmd Widget.EditionTreeMsg
            in
            ( { model
                  | basket = Nothing
                  , editorWidget = wid
                  , catalog = F.empty
                  , searchmode = newsearchmode
              }
            , Cmd.map WidgetMsg <| Cmd.batch [ cmd, byAndCmd ]
            )

        -- Expert mode

        WidgetMsg (Widget.NewFormula Nothing) ->
            U.nocmd model

        WidgetMsg (Widget.NewFormula (Just code)) ->
            case fromlisp code of
                Ok parsed ->
                    let
                        basketormula =
                            { code = code, node = parsed }
                    in
                    ( { model | basket = Just basketormula }
                    , tryfilter model basketormula
                    )
                Err err ->
                    U.nocmd model

        WidgetMsg x -> Tuple.mapBoth
            (\m -> { model | editorWidget = m })
            (Cmd.map WidgetMsg)
            (Widget.update x model.editorWidget)


viewnamefilter =
    let input =
            H.input
                [ A.class "input-group mb-1"
                , A.placeholder "filter by name"
                , A.title "You can type name fragments separated by spaces"
                , HE.onInput NameFilter
                ] []
    in
    H.map (provideInput >> DebounceNameFilter) input


viewformulafilter =
    let input =
            H.input
                [ A.class "input-group mb-1"
                , A.placeholder "filter on formula content"
               , A.title "You can type series name fragments and formula operator names"
                , HE.onInput FormulaFilter
                ] []
    in
    H.map (provideInput >> DebounceFormulaFilter) input


viewkindfilter model =
    let
        checkbox kind =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series type"]
                [ H.input
                    [ A.attribute "type" "checkbox"
                    , A.class "form-check-input"
                    , A.value kind
                    , A.checked <| List.member kind <| model.selectedkinds
                    , HE.onClick <| KindUpdated kind
                    ] []
                , H.label
                    [ A.class "form-check-label"
                    , A.for kind ]
                    [ H.text kind ]
                ]
    in
    H.div [] <|
        H.span
            [ A.class "font-italic" ]
            [ H.text "series kind → " ] :: (List.map checkbox [ "primary", "formula" ])


viewsourcefilter model =
    let
        checkbox source =
            H.div
                [ A.class "form-check form-check-inline"
                , A.title "filter by series source"]
                [ H.input
                      [ A.attribute "type" "checkbox"
                      , A.class "form-check-input"
                      , A.value source
                      , A.checked <| List.member source <| model.selectedsources
                      , HE.onClick <| SourceUpdated source
                      ] []
                , H.label
                      [ A.class "form-check-label"
                      , A.for source ]
                      [ H.text source ]
                ]
    in
    if List.length model.sources > 1
    then H.div []  <|
        H.span
            [ A.class "font-italic" ]
            [ H.text "series sources → " ] :: (List.map checkbox model.sources)
    else H.span [] []


metaactions : String -> (Int, (String, String)) -> H.Html Msg
metaactions action metadata =
    let
        inputid =
            U.first metadata
        key =
            U.first ( U.snd metadata )
        value =
            U.snd ( U.snd metadata )
        inputkey =
            H.input
                [ A.attribute "type" "text"
                , A.class "mb-1"
                , A.placeholder "filter by metadata key"
                , A.value key
                , HE.onInput (NewKey inputid)
                ] []
        inputvalue =
            H.input
                [ A.attribute "type" "text"
                , A.class "mb-1"
                , A.placeholder "filter by metadata value"
                , A.value value
                , HE.onInput (NewValue inputid)
                ] []
        addentry =
            H.button
                [ A.attribute "type" "button"
                , A.title "add the metadata filter rule"
                , A.class "btn btn-primary btn-sm"
                , HE.onClick (AddMetaItem inputid)
                ]
                [ H.text "add" ]
        delete =
            H.button
                [ A.attribute "type" "button"
                , A.title "remove the metadata filter rule"
                , A.class "btn btn-warning btn-sm"
                , HE.onClick (MetaItemToDelete inputid)
                ]
                [ H.text "delete" ]
        null =
            H.span [] []
    in
    H.div
        [ A.id <| String.fromInt inputid ]
        [ inputkey
        , H.span [] [ H.text " " ]
        , inputvalue
        , H.span [] [ H.text " " ]
        , if action == "add"
          then
              if (key /= "") && (key /= "") then addentry else null
          else delete
        ]


viewmetafilter model =
    let
        data =
            List.reverse <| Dict.toList model.filterbymeta
        header =
            Maybe.withDefault (1, ("", "")) <| List.head data
        tail =
            Maybe.withDefault [] <| List.tail data
    in
    H.div [] <|
        (metaactions "add" header):: List.map (metaactions "delete") tail


tzawarefilter model =
    let
        viewtzaware tzaware =
            let
                selected =
                    case model.tzaware of
                        Nothing -> "any"
                        Just aware ->
                            case aware of
                                True -> "true"
                                False -> "false"
            in
            H.option
                [ A.value tzaware
                , A.selected (selected == tzaware)
                ]
                [ H.text tzaware ]
    in
    H.div
        [ ]
        [ H.text "tzaware : "
        , H.select
            [ HE.targetValue
                |> D.map Tzaware
                |> HE.on "change"
            ]
            ( List.map viewtzaware [ "any", "true", "false" ] )
        ]


viewfilteredqty model =
    let
        len =
            List.length model.catalog.items

        msg =
            "Found " ++
            case len of
                0 -> "No item"
                1 -> "1 item"
                _ -> String.fromInt len ++ " items"

        msg2 =
            if len > 1000 then msg ++ " (only first 1000 items shown)." else msg ++ "."

    in
    H.p [] [ H.text msg2 ]


viewlimitwidget model =
    let
        strlimit =
            String.fromInt model.limit

        welcome =
            case model.limit of
                0 -> "There is currently no query limit."
                _ -> "The current query limit is set at " ++ strlimit ++ " items per source. "

        limitwidget =
            case model.editlimit of
                False ->
                    H.span []
                        [H.text welcome
                        , H.button
                            [ A.type_ "button"
                            , A.class "btn btn-secondary btn-sm"
                            , HE.onClick <| EditLimit model.editlimit ]
                            [ H.text "Edit the limit." ]
                        ]

                True ->
                    H.div []
                        [ H.input
                            [ HE.onInput EditedLimit
                            , A.value strlimit
                            ] []
                        , H.button
                            [ A.type_ "button"
                            , A.class "btn btn-secondary btn-sm"
                            , HE.onClick <| EditLimit model.editlimit
                            ]
                            [ H.text "Done editing." ]
                        ]
    in
    H.span [] [ limitwidget ]


viewfiltered baseurl mode catalog showsource =
    let
        makekey elt =
            elt.name ++ elt.source

        makeurl elt =
            UB.crossOrigin baseurl
                [ case mode of
                      Series -> "tsinfo"
                      Groups -> "groupinfo"
                ]
                [ UB.string "name" elt.name ]

        item elt =
            ( makekey elt
            , H.li
                [ A.class "list-group-item p-1" ]
                [ H.span
                      [ A.class <|
                            case elt.kind of
                                "formula" ->  "badge badge-success"
                                "bound" -> "badge badge-info"
                                _ -> "badge badge-secondary"
                      ]
                      [ H.text elt.kind ]
                , H.span [] [ H.text " " ]
                , H.a [ A.href (makeurl elt) ]
                    [ H.text elt.name ]
                , if showsource then
                      H.span
                          -- alas, Chrome does not know `inline-end`
                          [ A.style "float" "right" ]
                          [ H.span []
                                [ H.span
                                      [ A.class "badge badge-info" ]
                                      [ H.text elt.source ]
                                ]
                          ]
                  else
                      H.span [] []
                ]
            )

        items =
            List.map item catalog.items

        noseries = (List.length items) == 0

    in
    K.node "ul"
        [ A.class "list-group list-group-flush" ]
        items


viewerrors model =
    if List.length model.errors > 0 then
    H.div []
        [ H.h2 [] [ H.text "Errors" ]
        , H.div [] (List.map (\x -> H.p [] [ H.text x ]) model.errors)
        ]
    else H.span [] []


viewedition model =
    [ H.div
        [ A.class "mt-4" ]
        [ Widget.view model.editorWidget |> H.map WidgetMsg ]
    , viewerrors model
    ]



viewsearchmodetabs model =
    let
        tab searchmode label =
            H.li
                [ A.class "nav-item" ]
                [ H.a
                    [ A.class <| "nav-link" ++
                          if model.searchmode == searchmode then " active" else ""
                    , A.attribute "data-toggle" "tab"
                    , A.attribute "role" "tab"
                    , A.attribute "aria-selected"
                        <| if model.searchmode == searchmode then "true" else "false"
                    , A.id <| String.toLower label
                    , HE.onClick <| SetSearchMode searchmode
                    ]
                    [ H.text label ]
                ]
    in
    H.ul [ A.class "nav nav-tabs mb-3"
         , A.attribute "role" "tablist"
         ]
        [ tab Basic "Basic"
        , tab Expert "Expert"
        ]


viewsearchform model =
    H.div [ A.class "tab-content" ]
        [ H.div
            [ A.class <| "tab-pane"
                  ++ if model.searchmode == Basic then " active" else ""
            , A.id "basic"
            , A.attribute "role" "tabpanel"
            ]
            [ H.div
                [ A.class "tsview-form-input" ]
                [ H.div [] [ viewnamefilter ]
                , H.div [] [ viewformulafilter ]
                , viewmetafilter model
                , tzawarefilter model
                , viewkindfilter model
                , viewsourcefilter model
                , viewfilteredqty model
                , viewlimitwidget model
                ]
            ]
        , H.div
            [ A.class <| "tab-pane"
                  ++ if model.searchmode == Expert then " active" else ""
            , A.id "expert"
            , A.attribute "role" "tabpanel"
            ]
            [ H.div [ A.class "alert alert-info" ]
                <| viewedition model
            ]
        ]


view : Model -> H.Html Msg
view model =
    let
        nbsources =
            List.length model.sources

        mode =
            case model.mode of
                Series -> "Series"
                Groups -> "Groups"

        -- build a few stats about series and groups
        metatoint meta =
            case meta of
                M.MInt anint -> anint
                _ -> 0

        getint dict item =
            metatoint <| Maybe.withDefault (M.MInt 0) <| Dict.get item dict

        itemsum items dict =
            List.foldl (+) 0 <| List.map (getint dict) items

        seriesbysource source =
            itemsum [ "formula_series", "primary_series" ]
                <| Maybe.withDefault Dict.empty
                <| Dict.get source model.stats

        groupsbysource source =
            itemsum [ "formula_groups", "primary_groups", "bound_groups" ]
                <| Maybe.withDefault Dict.empty
                <| Dict.get source model.stats

        allthings thingsbysource =
            List.foldl (+) 0 <| List.map thingsbysource <| Dict.keys model.stats

    in
    H.div [ A.class "main-content" ]
        [ H.span
              [ HE.onClick ToggleMode
              , A.style "float" "right"
              ]
              [ H.div
                    [ A.class "form-check form-check-inline" ]
                    [ H.input [ A.class "form-check-input"
                              , A.type_ "radio"
                              , A.checked <| case model.mode of
                                                 Series -> True
                                                 Groups -> False
                              , A.name "doseries"
                              , A.id "doseries"] [ ]
                    , H.label
                        [ A.class "form-check-label"
                        , A.for "doseries"
                        ]
                        [ H.text "series" ]
                    ]
              , H.div [ A.class "form-check form-check-inline" ]
                  [ H.input  [ A.class "form-check-input"
                             , A.type_ "radio"
                             , A.checked <| case model.mode of
                                                Series -> False
                                                Groups -> True
                             , A.name "dogroups"
                             , A.id "dogroups" ] [ ]
                  , H.label
                      [ A.class "form-check-label"
                      , A.for "dogroups"
                      ]
                      [ H.text "groups" ]
                  ]
              , H.div []
                  [ H.text <| "Total " ++
                        (String.fromInt
                             <| allthings
                             <| case model.mode of
                                    Series -> seriesbysource
                                    Groups -> groupsbysource
                        )
                  ]
              ]
        , H.h1 [ A.class "page-title" ] [ H.text <| mode ++ " Catalog" ]
        , viewsearchmodetabs model
        , viewsearchform model
        , L.lazy4 viewfiltered model.baseurl model.mode model.catalog (nbsources > 1)
        , viewerrors model
        ]


type alias Input =
    { baseurl : String
    , queryspec : D.Value
    }


makemodel baseurl widget =
    let
        debouncerconfig =
            Debouncer.manual |>
            settleWhenQuietFor (Just <| fromSeconds 0.3) |>
            toDebouncer
    in
    { baseurl = baseurl
    , mode = Series
    , searchmode = Basic
    , stats = Dict.empty
    , sources = []
    , catalog = F.empty
    , limit = 1000
    , editlimit = False
    , selectedkinds = [ "primary", "formula" ]
    , selectedsources = []
    , filterbyname = Nothing
    , filterbyformula = Nothing
    , tzaware = Nothing
    , filterbymeta = Dict.empty
    , errors = []
    , namefilterdeb = debouncerconfig
    , formulafilterdeb = debouncerconfig
    , editorWidget = widget
    , basket = Nothing
    }


init : Input -> ( Model, Cmd Msg )
init { baseurl, queryspec } =
    let
        ( widget, widgetCmd ) =
            Widget.init
            { urlPrefix = baseurl
            , jsonSpec = queryspec
            , formulaCode = Nothing
            , returnTypeStr = "query"
            }

        model =
            makemodel baseurl widget
    in
    ( model
    , Cmd.batch
        [ Cmd.map WidgetMsg widgetCmd
        , getsources model.baseurl
        , U.getinfo model GotInfo
        , Cmd.map GotItemsDesc <|
            F.find baseurl "series" F.ReceivedSeries
                (query model) model.selectedsources model.limit
        ]
    )


main : Program Input Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
