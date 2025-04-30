module Search exposing (main)

import Browser
import Debouncer.Messages as Debouncer exposing
    ( Debouncer
    , fromSeconds
    , provideInput
    , settleWhenQuietFor
    , toDebouncer
    )
import Dict exposing (Dict)
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
import Lisp as L
import List.Extra as LE
import Metadata as M
import Set exposing (Set)
import Url.Builder as UB
import Util as U


type Mode =
    Series | Groups


type alias Model =
    { baseurl : String
    , mode : Mode
    -- base catalog elements
    , sources : List String
    , catalog : F.Model
    -- filter state (series)
    , selectedkinds : List String
    , selectedsources : List String
    -- filter state (all)
    , filterbyname : Maybe String
    , filterbyformula : Maybe String
    , tzaware : String
    -- filter state/metadata
    , filterbymeta : Dict Int (String, String)
    , errors : List String
    -- debouncing
    , namefilterdeb : Debouncer Msg
    , formulafilterdeb : Debouncer Msg
    }


type Msg
    = GotSources (Result Http.Error String)
    | GotItemsDesc F.Msg
    | NameFilter String
    | FormulaFilter String
    | KindUpdated String
    | SourceUpdated String
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
                Just fragment -> [ "(by.name " ++ quote ++ fragment ++ quote ++ ")" ]

        byformula =
            case model.filterbyformula of
                Nothing -> []
                Just fragment -> [ "(by.formulacontents " ++ quote ++ fragment ++ quote ++ ")" ]

        bytzaware =
            case model.tzaware of
                "true" -> [ "(by.tzaware)" ]
                "false" -> [ "(by.not (by.tzaware))" ]
                _ -> []

        filterfrommeta (key, value) =
            case value of
                "" ->
                    "(by.metakey " ++ quote ++ key ++ quote ++ ")"

                _ ->
                    "(by.metaitem " ++
                        quote ++ key ++ quote ++ " " ++
                        quote ++ value ++ quote ++ ")"

        bymeta =
            List.map filterfrommeta <| Dict.values model.filterbymeta

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
                            (query model) model.selectedsources
                Groups ->
                    Cmd.map GotItemsDesc <|
                        F.find model.baseurl "group" F.ReceivedGroups
                            (query model) model.selectedsources
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
                    then remove model.selectedkinds kind
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

        -- metadata filtering

        NewValue inputId value ->
            let
                currentkey =
                    U.first
                        (Maybe.withDefault
                             ( "", "" )
                             (Dict.get inputId model.filterbymeta)
                        )

                newdict =
                    Dict.insert
                        inputId
                        (currentkey, value)
                        model.filterbymeta

                newmodel =
                    { model | filterbymeta = newdict }
            in
            doquery newmodel

        NewKey inputId key ->
            let
                currentvalue =
                    U.snd
                        (Maybe.withDefault
                             ( "", "" )
                             (Dict.get inputId model.filterbymeta)
                        )

                newdict =
                    Dict.insert
                        inputId
                        (key, currentvalue)
                        model.filterbymeta

                newmodel =
                    { model | filterbymeta = newdict }
            in
            doquery newmodel

        AddMetaItem inputId ->
            let
                newdict =
                    Dict.insert
                        (inputId + 1)
                        ( "", "" )
                        model.filterbymeta

                newmodel =
                    { model | filterbymeta = newdict }
           in
           doquery newmodel

        MetaItemToDelete inputId ->
            let
                newdict =
                    Dict.remove inputId model.filterbymeta

                newmodel =  { model | filterbymeta = newdict }
            in
            doquery newmodel

        Tzaware value ->
            doquery { model | tzaware = value }

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


viewnamefilter =
    let input =
            H.input
                [ A.class "form-control-sm"
                , A.placeholder "filter by name"
                , A.size 80
                , A.title "You can type name fragments separated by spaces"
                , HE.onInput NameFilter
                ] []
    in
    H.map (provideInput >> DebounceNameFilter) input


viewformulafilter =
    let input =
            H.input
                [ A.class "form-control-sm"
                , A.placeholder "filter on formula content"
                , A.size 80
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
        inputId = U.first metadata
        key = U.first ( U.snd metadata )
        value = U.snd ( U.snd metadata )
        keyInput = H.input
            [ A.attribute "type" "text"
                , A.class "form-control-sm"
                , A.placeholder "filter by metadata key"
                , A.value key
                , HE.onInput (NewKey inputId)
            ]
            []
        valueInput = H.input
            [ A.attribute "type" "text"
                , A.class "form-control-sm"
                , A.placeholder "filter by metadata value"
                , A.value value
                , HE.onInput (NewValue inputId)
            ]
            []
        addentry =
            H.button
                [ A.attribute "type" "button"
                , A.title "add the metadata filter rule"
                , A.class "btn btn-primary btn-sm"
                , HE.onClick (AddMetaItem inputId)
                ]
                [ H.text "add" ]
        delete =
            H.button
                [ A.attribute "type" "button"
                , A.title "remove the metadata filter rule"
                , A.class "btn btn-warning btn-sm"
                , HE.onClick (MetaItemToDelete inputId)
                ]
                [ H.text "delete" ]
    in  H.div
        [ A.id (String.fromInt inputId) ]
        [ keyInput
        , H.span [] [ H.text " " ]
        , valueInput
        , H.span [] [ H.text " " ]
        , if action == "add" then addentry else delete
        ]


viewmetafilter model =
    let
        listData = List.reverse (Dict.toList model.filterbymeta)
        header = Maybe.withDefault (1, ("", "")) (List.head listData)
        tail = Maybe.withDefault [] (List.tail listData)

    in
    H.div
        []
        ((metaactions "add" header):: List.map (metaactions "delete") tail)


viewfilteredqty model =
    let
        len =
            List.length model.catalog.items

        msg =
            case len of
                0 -> "No item"
                1 -> "1 item"
                _ -> String.fromInt len ++ " items"

        msg2 =
            if len > 1000 then msg ++ " (only first 1000 items shown)." else msg ++ "."

    in
    H.p [] [ H.text ("Found " ++ msg2) ]


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


view : Model -> H.Html Msg
view model =
    let
        nbsources =
            List.length model.sources

        mode =
            case model.mode of
                Series -> "Series"
                Groups -> "Groups"

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
              ]
        , H.h1 [ A.class "page-title" ] [ H.text <| mode ++ " Catalog" ]
        , H.div
              [ A.class "tsview-form-input" ]
              [ H.div [] [ viewnamefilter ]
              , H.div [] [ viewformulafilter ]
              , viewmetafilter model
              , tzawareDropdown model
              , viewkindfilter model
              , viewsourcefilter model
              , viewfilteredqty model
              ]
        , L.lazy4 viewfiltered model.baseurl model.mode model.catalog (nbsources > 1)
        , viewerrors model
        ]


tzawareDropdown : Model -> H.Html Msg
tzawareDropdown model =
    let
        decodetzaware : String -> D.Decoder String
        decodetzaware tzaware =
            D.succeed tzaware

    in
    H.div
        [ ]
        [ H.text "tzaware : "
        , H.select
            [ HE.targetValue
                |> D.andThen decodetzaware
                |> D.map Tzaware
                |> HE.on "change" ]
            (List.map (renderTzaware model.tzaware) ["any", "true", "false"])
        ]

renderTzaware : String -> String -> H.Html Msg
renderTzaware selectedTzaware tzaware =
    H.option
        [ A.value tzaware
        , A.selected (selectedTzaware == tzaware)
        ]
        [ H.text tzaware ]


type alias Input =
    { baseurl : String }


main : Program Input Model Msg
main =
       let
           debouncerconfig =
               Debouncer.manual |>
               settleWhenQuietFor (Just <| fromSeconds 0.3) |>
               toDebouncer

           makemodel input =
               { baseurl = input.baseurl
               , mode = Series
               , sources = []
               , catalog = F.empty
               , selectedkinds = [ "primary", "formula" ]
               , selectedsources = []
               , filterbyname = Nothing
               , filterbyformula = Nothing
               , tzaware = "any"
               , filterbymeta = Dict.empty
               , errors = []
               , namefilterdeb = debouncerconfig
               , formulafilterdeb = debouncerconfig
               }

           init input =
               let
                   model =
                       makemodel input
               in
               ( model
               , Cmd.batch
                   [ getsources model.baseurl
                   , Cmd.map GotItemsDesc <|
                       F.find input.baseurl "series" F.ReceivedSeries
                           (query model) model.selectedsources
                   ]
               )

           sub _ = Sub.none
       in
           Browser.element
               { init = init
               , view = view
               , update = update
               , subscriptions = sub
               }
